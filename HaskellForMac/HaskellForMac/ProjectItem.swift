//
//  ProjectItem.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Data structure that holds the information of the various entities that make up a project.

import Foundation
import Quartz
import TacticalBase


/// The toplevel groups
///
/// NB: The raw value determines the position of a group in the outline view.
///
public enum ProjectGroupCategory: Int {
  case Package = 0, Executable, ExtraSource, Data
}

extension ProjectGroupCategory: Printable {

  /// Corresponding outline view group ID.
  ///
  func groupID() -> String {
    switch self {
    case .Package:     return "Project information"
    case .Executable:  return "Programs"
    case .ExtraSource: return "Non-Haskell sources"
    case .Data:        return "Supporting files"
    }
  }

  public var description: String { get { return self.groupID() } }
}

/// Information for the various forms of files that can be part of a project
///
public enum ProjectFileCategory {

    // Haskell module that is part of the package (might be the main module of an executable)
  case Haskell(isMainFile: Bool, playground: ProjectViewModelPlayground)

    // Non-Haskell file that is part of the package
  case Other
}

/// Informations that is specific to a single category of project items.
///
public enum ProjectItemCategory {

    // Toplevel group in the project source view
  case Group(category: ProjectGroupCategory)

    // Package information (representative of the project's Cabal file)
  case Package

    // Represents a single executable secion in the Cabal file
  case Executable

    // Logical collection of files (wildcard name or source directory or data directory)
  case FileGroup

    // Represents a directory in the file system
  case Folder

    // Represents a regular file in the file system
  case File(details: ProjectFileCategory)

  func helpTip() -> String {
    switch self {
    case .Group(category: .Package):     return "Haskell project package"
    case .Group(category: .Data):        return "Data files included in this project"
    case .Group(category: .Executable):  return "Executable products produced by this project"
    case .Group(category: .ExtraSource): return "Source code in languages other than Haskell (including mark up languages)"
    case .Package:                       return "Haskell project package"
    case .Executable:                    return "Executable program generated from subordinate Haskell modules"
    case .FileGroup:                     return "Group of Haskell modules"
    case .Folder:                        return "Folder"
    case .File(details: .Haskell(let isMain, let playground)):
                                         if isMain { return "Main Haskell module of enclosing executable" }
                                         else { return "Haskell module" }
    case .File(details: .Other):         return "Supporting file"
    }
  }
}

/// Notification for when the entire file contents changes due to update to the file wrapper, such as an underlying
/// file system change.
///
typealias WrapperChangeNotification = String -> ()

/// Each instance of this class represents one entity within the project.
///
/// Invariants not captured in the type structure:
///
///   * An item has *either* `children` *or* a `fileContent`. (We return "" as the file content for items with children
///     and ignore any updates.)
///   * If an item with `children` is associated with a file wrapper, it is always a directory file wrapper.
///   * If an item with a `fileContents` is associated with a file wrapper, it is always a regular file wrapper.
///
public final class ProjectItem: NSObject {

  public let category:   ProjectItemCategory   // The flavour of the item and flavour-specific information
  public var identifier: String                // Identifier that is used to display the item — mutable to rename

  /// The project view model that this item belongs to. (Weak reference as this item is owned by the model.)
  ///
  public weak var viewModel: HFMProjectViewModel!

  public weak var parent:      ProjectItem?    // Current parent item unless `catagory` is `.Group`
  public      var children:    [ProjectItem]   // All items that comprise the content of the current one
  public      var fileWrapper: NSFileWrapper?  // Associated file wrapper containing the contents represented on the file
                                               // system. It is `nil` iff item not yet in project file wrapper structure.

  private         var dirtyFileContents: String?  // Non-nil if associated file was changed & still needs to be saved.
  public  dynamic var fileContents:      String { // KVO complaint access to the file contents associated with the item
    get {
      if let fileContents = dirtyFileContents { return fileContents }
      if let wrapper = fileWrapper {
        if wrapper.regularFile {
          if let data = wrapper.regularFileContents {
            return String(NSString(data: data, encoding: NSUTF8StringEncoding) ?? "")
          } else { return "" }
        } else { return "" }
      } else { return "" }
    }
    set { dirtyFileContents = newValue }
  }

  /// Registered notifications.
  ///
  /// NB: The call back functions are weak references, they may go at any time, which implicitly unregisters them.
  ///
  private var wrapperChangeNotifications: [WeakApply<WrapperChangeNotification>]  = []


  // MARK: -
  // MARK: Initialisation

  /// Designated initialiser setting the passed in values.
  ///
  /// To build the recursive parent-child relation, we pass in a builder function for the children that needs to be
  /// applied to the parent to obtain the children.
  ///
  init(itemCategory: ProjectItemCategory,
       identifier:   String,
       viewModel:    HFMProjectViewModel,
       parent:       ProjectItem?,
       fileWrapper:  NSFileWrapper?,
       children:     ProjectItem -> [ProjectItem])
  {
    self.category          = itemCategory
    self.identifier        = identifier
    self.viewModel         = viewModel
    self.parent            = parent
    self.fileWrapper       = fileWrapper
    self.dirtyFileContents = nil
    self.children          = []               // break the recursive dependency
    super.init()
    self.children          = children(self)
  }

  /// Convenience initialiser for group items that recursively creates the entire item tree for the given group.
  ///
  convenience init(groupCategory: ProjectGroupCategory, viewModel: HFMProjectViewModel) {
    self.init(itemCategory: .Group(category: groupCategory),
              identifier: groupCategory.groupID(),
              viewModel: viewModel,
              parent: nil,
              fileWrapper: viewModel.fileWrapper,
              children: { parent in groupItemChildren(groupCategory, parent) })
  }

  /// Dummy instance.
  ///
  convenience override init() {
    self.init(itemCategory: .Group(category: .Package),
              identifier: "dummy",
              viewModel: HFMProjectViewModel(),
              parent: nil,
              fileWrapper: nil,
              children: const([]))
  }
}

/// Compute the children of the group represented the given `parent`, which is of the given category.
///
private func groupItemChildren(groupCategory: ProjectGroupCategory, parent: ProjectItem) -> [ProjectItem] {
  switch groupCategory {

    // Package group: 1 child: package header
  case .Package:
    if let fileWrapper = parent.viewModel.fileWrapper.fileWrappers[parent.viewModel.cabalFileName] as? NSFileWrapper {

      return [ProjectItem(itemCategory: .Package,
                          identifier: parent.viewModel.identifier,
                          viewModel: parent.viewModel,
                          parent: parent,
                          fileWrapper: fileWrapper,
                          children: const([]))]

    } else {
      NSLog("%s: cabal file wrapper with filename '%@' disappeared", __FUNCTION__, parent.viewModel.cabalFileName)
      return []
    }

    // Executable group: 1 child: executable section
  case .Executable:
    return [ProjectItem(itemCategory: .Executable,
                        identifier: parent.viewModel.executableName,
                        viewModel: parent.viewModel,
                        parent: parent,
                        fileWrapper: parent.viewModel.fileWrapper,   // this is the filer wrapper containing the Haskell modules
                        children: executableChildren)]

    // Extra source group: multiple files in a folder hierarchy
  case .ExtraSource:
    return childrenFromDictionary(parent.viewModel.fileWrapper,
                                  (parent.viewModel.extraSrcFiles as? [String: AnyObject])!,
                                  false,
                                  parent.viewModel,
                                  parent)

    // Data group: depending on whether 'data-dir:' is defined, we either...
  case .Data:
    if let dataDir = parent.viewModel.dataDir  {   // ...start with a file group or...

      // File group: 1 child: 'data-dir:'
      let dataDirFileWrapper = fileWrapperForFileGroup(parent.viewModel.fileWrapper, dataDir)
      let makeChildren       = { newParent in childrenFromDictionary(dataDirFileWrapper,
                                                                     (parent.viewModel.dataFiles as? [String: AnyObject])!,
                                                                     false,
                                                                     parent.viewModel,
                                                                     newParent) }
      return [ProjectItem(itemCategory: .FileGroup,
                          identifier: dataDir,
                          viewModel: parent.viewModel,
                          parent: parent,
                          fileWrapper: dataDirFileWrapper,
                          children: makeChildren)]

    } else {                                      // ...directly descent into the hierarchy of the data files.

      // Data files group: multiple files in a folder hierarchy
      return childrenFromDictionary(parent.viewModel.fileWrapper,
                                    (parent.viewModel.dataFiles as? [String: AnyObject])!,
                                    false,
                                    parent.viewModel,
                                    parent)
    }
  }
}

/// Compute the children of the executable group item represented by `parent`.
///
private func executableChildren(parent: ProjectItem) -> [ProjectItem] {

  // Depending on whether 'source-dir:' is defined, we either...
  if let sourceDir = parent.viewModel.sourceDir {   // ...start with a file group or...

    // File group: 1 child: 'source-dir:'
    let sourceDirFileWrapper = fileWrapperForFileGroup(parent.viewModel.fileWrapper, sourceDir)
    let makeChildren         = { newParent in childrenFromDictionary(sourceDirFileWrapper,
                                                                     (parent.viewModel.modules as? [String: AnyObject])!,
                                                                     true,
                                                                     parent.viewModel,
                                                                     newParent) }
    return [ProjectItem(itemCategory: ProjectItemCategory.FileGroup,
                        identifier: sourceDir,
                        viewModel: parent.viewModel,
                        parent: parent,
                        fileWrapper: sourceDirFileWrapper,
                        children: makeChildren)]

  } else {                                   // ...directly display the main module and other modules.

    return childrenFromDictionary(parent.viewModel.fileWrapper,
                                  (parent.viewModel.modules as? [String: AnyObject])!,
                                  true,
                                  parent.viewModel,
                                  parent)
  }
}


// MARK: -
// MARK: Auxiliary functions traversing file wrapper trees

/// Compute an array of child items from their path dictionary and file wrapper structure. The children in the returned
/// array are sorted by name.
///
func childrenFromDictionary(rootFileWrapper: NSFileWrapper,
                            pathDict:        [String: AnyObject],
                            asSourceModules: Bool,
                            viewModel:       HFMProjectViewModel,
                            parent:          ProjectItem        ) -> [ProjectItem]
{
  let haskellFileExtension    = HFMProjectViewModel.haskellFileExtension()
  var children: [ProjectItem] = []

  for (name, subdict) in pathDict {

    let nameDict = (subdict as? [String: AnyObject])!

    var identifier = name
    let isFolder   = nameDict.count > 0
    let isMainFile = !isFolder && name.pathExtension == haskellFileExtension
    var playground: ProjectViewModelPlayground?

    if (asSourceModules && !isFolder) {     // We have got a Haskell source file

        // Only the main file has an extension in the model => add it to all other sources
      if !isMainFile { identifier = name.stringByAppendingPathExtension(haskellFileExtension)! }

        // Get the associated playground
      if let playgroundFilename = ProjectViewModelPlayground.implicitPlaygroundFilename(identifier) {
        if let playgroundFileWrapper = rootFileWrapper.fileWrappers[playgroundFilename] as? NSFileWrapper {

          playground = ProjectViewModelPlayground(fileWrapper: playgroundFileWrapper, model: viewModel)

        } else {

          playground = ProjectViewModelPlayground(identifier: identifier, model: viewModel)

        }
      }
    }
    let fileDetails: ProjectFileCategory = (asSourceModules && playground != nil)
                                           ? .Haskell(isMainFile: isMainFile, playground: playground!)
                                           : .Other
    let category: ProjectItemCategory    = isFolder ? .Folder
                                                    : .File(details: fileDetails)
    if let fileWrapper = rootFileWrapper.fileWrappers[identifier] as? NSFileWrapper {

      let makeChildren = { newParent in
                             childrenFromDictionary(fileWrapper, nameDict, asSourceModules, viewModel, newParent)}
      children.append(ProjectItem(itemCategory: category,
                                  identifier: identifier,
                                  viewModel: viewModel,
                                  parent: parent,
                                  fileWrapper: fileWrapper,
                                  children: makeChildren))

    } else {

        // This shouldn't happen...
      children.append(ProjectItem(itemCategory: category,
                                  identifier: identifier,
                                  viewModel: viewModel,
                                  parent: parent,
                                  fileWrapper: nil,
                                  children: const([])))
    }
  }
  return children.sorted{ l, r in l.identifier <= r.identifier}
}

/// Given a wildcard pattern for a file group and the file wrapper at which it is rooted, return the file wrapper that
/// contains the files in the file group. (If the path doesn't exit in the file wrapper structure, return the first
/// argument.)
///
public func fileWrapperForFileGroup(rootFileWrapper: NSFileWrapper, wildcardPath: String) -> NSFileWrapper {

  let wildcardChars               = NSCharacterSet(charactersInString: "*")
  var fileWrapper: NSFileWrapper? = rootFileWrapper

    // Chase the path components down the directory hierarchy.
  for component in wildcardPath.pathComponents {

    if let wrapper = fileWrapper {

        // File groups that include wildcards are always filenames (not just directory names) and must disregard the
        // filename wildcard component to determine their directory wrapper.
      let wildcardRange = component.rangeOfCharacterFromSet(wildcardChars)
      if wildcardRange != nil { break }

      fileWrapper = wrapper.fileWrappers[component] as? NSFileWrapper

    }

  }
  return fileWrapper ?? rootFileWrapper
}


// MARK: -
// MARK: Queries

extension ProjectItem {

  /// Determines whether the item represents a regular file on the file system.
  ///
  var regularFile: Bool { get {
    switch category {
    case .File(details: let details): if let wrapper = fileWrapper { return wrapper.regularFile } else { return true }
    default:                          return false
    } } }

  /// Returns the item's file path *relative* to the document root *if* the item is associated with a regular file or
  /// directory.
  ///
  var filePath: String? { get {
    if !(regularFile || isPackage) { return nil }

    var path:    [String]     = []
    var current: ProjectItem? = self

    while let item = current {
      switch item.category {
      case .Package:                       path.insert(viewModel.cabalFileName, atIndex: 0)
      case .FileGroup:                     path.insert(item.identifier, atIndex: 0)
      case .Folder:                        path.insert(item.identifier, atIndex: 0)
      case .File(details: let details):    path.insert(item.identifier, atIndex: 0)
      case .Group(category: let category): ()
      case .Executable:                    ()
      }
      current = item.parent
    }
    if path.count > 0 { return NSString.pathWithComponents(path) } else { return nil }
    } }

  /// Returns the URL from which this item was loaded *if* the item is associated with a regular file.
  ///
  /// WARNING: Use this with care. The contents of the referenced file is only valid if the project was saved (i.e., 
  ///          isn't dirty)!
  ///
  var URL: NSURL? { get {
    if let path = filePath { return viewModel.documentURL.URLByAppendingPathComponent(path) } else { return nil }
    } }

  /// Determines whether the item has unsaved changes.
  ///
  var isDirty: Bool { get { return dirtyFileContents != nil } }

  /// Help tip of the category.
  ///
  var helpTip: String { get { return category.helpTip() } }

  /// The index of the item in the parent's array of children.
  ///
  var index: Int { get {
    if let parent = parent {
      return find(parent.children, self) ?? -1
    } else {
      return groupCategory?.rawValue ?? 0
    } }
  }
}

// Queries to quickly test categories.
//
extension ProjectItem {

  public var isGroup: Bool {
    switch category {
    case .Group(_): return true
    default:        return false
    } }

  /// Determine the group category in which the item is located in the view model.
  ///
  public var groupCategory: ProjectGroupCategory? {
    switch category {
    case .Group(category: let category): return category
    default:                             if let parent = parent { return parent.groupCategory } else { return nil }
    } }

  /// Whether the item is located in the executable group.
  ///
  public var isInExecutableCategory: Bool {
    switch groupCategory {
    case .Some(.Executable): return true
    default:                 return false
    } }

  /// Whether the item is located in the extra source group.
  ///
  public var isInExtraSourceCategory: Bool {
    switch groupCategory {
    case .Some(.ExtraSource): return true
    default:                  return false
    } }

  /// Whether the item is located in the data group.
  ///
  public var isInDataCategory: Bool {
    switch groupCategory {
    case .Some(.Data): return true
    default:           return false
    } }

  public var isPackageGroup: Bool {
    switch category {
    case .Group(category: .Package): return true
    default:                         return false
    } }

  public var isExecutableGroup: Bool {
    switch category {
    case .Group(category: .Executable): return true
    default:                            return false
    } }

  public var isExtraSourceGroup: Bool {
    switch category {
    case .Group(category: .ExtraSource): return true
    default:                             return false
    } }

  public var isDataGroup: Bool {
    switch category {
    case .Group(category: .Data): return true
    default:                      return false
    } }

  public var isPackage: Bool {
    switch category {
    case .Package: return true
    default:       return false
    } }

  public var isExecutable: Bool {
    switch category {
    case .Executable: return true
    default:          return false
    } }

  public var isFileGroup: Bool {
    switch category {
    case .FileGroup: return true
    default:         return false
    } }

  public var isFolder: Bool {
    switch category {
    case .Folder: return true
    default:      return false
    } }

  public var isFile: Bool {
    switch category {
    case .File(details: _): return true
    default:                return false
    } }

  public var isMainFile: Bool {
    switch category {
    case .File(details: .Haskell(isMainFile: let isMainFile, playground: _)) where isMainFile: return true
    default:                                                                                   return false
    } }

  public var playground: ProjectViewModelPlayground? {
    switch category {
    case .File(details: .Haskell(isMainFile: let _, playground: let playground)): return playground
    default:                                                                      return nil
    } }

  public var isEmptyFolder: Bool {
    switch category {
    case .Folder, .FileGroup: return children.map{$0.isEmptyFolder}.reduce(true){$0 && $1}
    default: return false
    } }

  public var isDirectory: Bool {                   // I.e., may have files as children
    return isExtraSourceGroup || isDataGroup || isExecutable || isFolder || isFileGroup
    }

  /// Is the main file among the current item's subitems.
  ///
  public var containsMainFile: Bool {
    return children.map{ $0.isMainFile || $0.containsMainFile }.reduce(false){$0 || $1}
    }
}


// MARK: -
// MARK: Item status changes

extension ProjectItem {

  /// Objects (e.g., editors) register to be notified of item changes. The notification is automatically
  /// deregistered when the object gets deallocated. The callback will be executed right away with the initial value.
  ///
  /// We keep a strong reference to the callback functions, but *not* to the object.
  ///
  func reportItemChanges<S: AnyObject>(object:                    S,
                                       wrapperChangeNotification: S -> WrapperChangeNotification)
  {
    wrapperChangeNotifications.append(WeakApply(wrapperChangeNotification, object))
    wrapperChangeNotification(object)(fileContents)
  }

  /// Invoke all register callbacks waiting for font changes.
  ///
  private func notifyWrapperChange(newContents: String)
  {
    for notification in wrapperChangeNotifications {
      if let callback = notification.unbox { callback(newContents) }
    }

    // Prune stale notification callbacks.
    wrapperChangeNotifications = wrapperChangeNotifications.filter{ $0.unbox != nil }
  }
}


// MARK: -
// MARK: Flushing changes

extension ProjectItem {

  /// Replace this item's file wrapper with a new one and discard any uncommitted changes.
  ///
  /// This includes relacing the file wrapper in the file wrapper structure; i.e.; the item is clean afterwards.
  ///
  func replaceFileWrapper(newFileWrapper: NSFileWrapper) {
    let oldFileWrapper = fileWrapper
    dirtyFileContents  = nil
    fileWrapper        = newFileWrapper

    if let parent = parent {
      if let oldFileWrapper = oldFileWrapper { parent.fileWrapper?.removeFileWrapper(oldFileWrapper) }
      parent.fileWrapper?.addFileWrapper(newFileWrapper)
    }

    notifyWrapperChange(fileContents)
  }

  /// Produce a file wrapper for the item that (a) has all pending changes applied and (b) is properly referenced by
  /// the given parent file wrapper.
  ///
  func cleanFileWrapperWithParent(parentFileWrapper: NSFileWrapper) -> NSFileWrapper? {
    if !parentFileWrapper.directory { return nil }

    switch category {

    case .Group(_), .Executable:                                // Virtual container items
      return fileWrapper

    case .FileGroup, .Folder:                                   // Items representing directories
      if let wrapper = fileWrapper { return wrapper }

        // Directory represented by `item` is not yet in the file wrapper tree => insert a new empty directory wrapper
      let wrapper               = NSFileWrapper(directoryWithFileWrappers: [:])
      wrapper.preferredFilename = identifier
      fileWrapper               = wrapper
      parentFileWrapper.addFileWrapper(wrapper)
      return wrapper
      
    case .Package, .File(_):                                    // Items representing files
      if fileWrapper == nil && dirtyFileContents == nil { dirtyFileContents = "" }

        // File has changed => replace the old file wrapper by a new one with the new contents
      if let newFileContents = dirtyFileContents {

        dirtyFileContents = nil
        if let data = newFileContents.dataUsingEncoding(NSUTF8StringEncoding) {

          if let oldFileWrapper = fileWrapper { parentFileWrapper.removeFileWrapper(oldFileWrapper) }
          let filename = isPackage ? viewModel.cabalFileName : identifier
          let name     = parentFileWrapper.addRegularFileWithContents(data, preferredFilename: filename)
          fileWrapper = (parentFileWrapper.fileWrappers[name] as? NSFileWrapper)!

        } else { NSLog("%@: can't encode file '$@'", __FUNCTION__, identifier) }

      }
      return fileWrapper
    }
  }
}


// MARK: -
// MARK: Edits

extension ProjectItem {

  /// Create a new empty file for regular file items whose file wrapper is `nil`. (These are files that are listed in
  /// the Cabal file, but do not exist in the file system.)
  ///
  func touch() {
    if !regularFile { return }

      // Be careful to not overwrite any existing file contents.
    if fileWrapper == nil && dirtyFileContents == nil { dirtyFileContents = fileContents }
  }

  /// Copy the given file into the current item (which must be a folder or group) at the given child index.
  ///
  /// NB: Overwrites any existing file at the destination without further checking.
  ///
  func copyFileAtURL(url: NSURL, toIndex index: Int, error: NSErrorPointer) -> Bool {
    if !isDirectory || fileWrapper == nil { NSLog("%@: bad parent item", __FUNCTION__); return false }
    if children.count < index { NSLog("%@: bad index: %@", __FUNCTION__, index); return false }
    if url.filePathURL == nil { NSLog("%@: not a filepath URL: %@", __FUNCTION__, url); return false }
    if let ext = url.pathExtension {
      if isInExecutableCategory && ext != HFMProjectViewModel.haskellFileExtension() {
        error.memory = NSError(domain: "Only Haskell files can be part of an executable", code: -1, userInfo: nil)
        return false
      }
    }

    if let newIdentifier = url.lastPathComponent {

      let newFileWrapper = NSFileWrapper(URL: url, options: NSFileWrapperReadingOptions.Immediate, error: error)
      if let newFileWrapper = newFileWrapper {

        if let overwrittenItem = children.filter({ $0.identifier == newIdentifier }).first {

          overwrittenItem.replaceFileWrapper(newFileWrapper)

        } else {

            // Create a new item of the appropriate type for this file wrapper and add it into the item and file wrapper
            // structures.
          var details: ProjectFileCategory
          if isInExecutableCategory {

            let newPlayground = ProjectViewModelPlayground(identifier: newIdentifier, model: viewModel)
            details           = .Haskell(isMainFile: false, playground: newPlayground!)

          } else {
            details = .Other
          }
          let newChild = ProjectItem(itemCategory: .File(details: details),
                                     identifier: newIdentifier,
                                     viewModel: viewModel,
                                     parent: self,
                                     fileWrapper: newFileWrapper,
                                     children: const([]))
          children.insert(newChild, atIndex: index)
          fileWrapper?.addFileWrapper(newFileWrapper)

        }
        return true

      } else { return false }

    } else { NSLog("%@: invalid source URL: %@", __FUNCTION__, url); return false }
  }

  /// Create a new item of the current item at the given child index.
  ///
  /// `folderWanted` indicates whether the item should represent a file or folder. Depending on the group into which the
  /// the item is inserted, files will be created as Haskell files or plain text files.
  ///
  func newItemAtIndex(index: Int, folder folderWanted: Bool) -> Bool {
    if !isDirectory || fileWrapper == nil || (isGroup && isExecutableGroup) { return false }
    if children.count < index { return false }

    let createModule   = !folderWanted && isInExecutableCategory
    let baseName       = createModule ? "NewModule" : (folderWanted ? "NewFolder" : "NewText")
    let fileExtension  = createModule ? HFMProjectViewModel.haskellFileExtension()
                                      : (folderWanted ? nil : HFMProjectViewModel.textFileExtension())

      // Determine the name of the new item.
    let usedNames      = children.map{$0.identifier.stringByDeletingPathExtension}
    let baseIdentifier = nextName(baseName, usedNames)
    let identifier     = (fileExtension == nil) ? baseIdentifier
                                                : baseIdentifier.stringByAppendingPathExtension(fileExtension)!

      // Determine the item category.
    let newPlayground = createModule ? ProjectViewModelPlayground(identifier: identifier, model: viewModel) : nil
    let details       = (newPlayground == nil) ? ProjectFileCategory.Other
                                               : ProjectFileCategory.Haskell(isMainFile: false, playground: newPlayground!)
    let category      = folderWanted ? ProjectItemCategory.Folder
                                     : ProjectItemCategory.File(details: details)

      // Create the item.
    let newChild      = ProjectItem(itemCategory: category,
                                    identifier: identifier,
                                    viewModel: viewModel,
                                    parent: self,
                                    fileWrapper: nil,
                                    children: const([]))
    children.insert(newChild, atIndex: index)
    if !folderWanted { newChild.fileContents = "" }                     // This marks file items as dirty.
    return true
  }

  /// Move the current item from the view model and the file wrapper structure.
  ///
  /// NB: Force a save right away to update the file system!
  ///
  /// @Returns: Was the operation sucessful?
  ///
  func remove() -> Bool {
    if isMainFile { return false }      // We cannot delete the Main file

    if containsMainFile {
      // FIXME: We need to move the main file to be our sibling. Then, delete as usual. (Issue #164)
      NSLog("Cannot delete folders containing the main file yet (Issue #164)")
      return false
    }

      // Remove the associated playground if any.
    if let playground = playground {
      playground.remove(identifier, parent:(parent?.fileWrapper != nil) ? parent!.fileWrapper! : viewModel.fileWrapper)
    }

      // Remove this item from the hierarchy.
    return parent!.removeChild(self)
  }

  private func removeChild(childItem: ProjectItem) -> Bool {

      // Remove the model view item.
    if !contains(children, childItem) { return false }
    children = children.filter{ $0 !== childItem }

      // Remove the associated file wrapper.
    if let wrapper = fileWrapper {
      if let childWrapper = childItem.fileWrapper { wrapper.removeFileWrapper(childWrapper) }
    }
    return true
  }

  /// Attempt to rename the current item with the given identifier. If the result is non-nil, the given identifier was
  /// rejected and the returned one used instead.
  ///
  func renameTo(newIdentifier: String) -> String? {
    if parent == nil { return nil }

    let parentFileWrapper = (parent!.fileWrapper != nil) ? parent!.fileWrapper! : viewModel.fileWrapper

      // Make sure the new identifier is unique (although, `self.fileWrapper` should not be `nil`, it seemed tricky in
      // the past to just let the file wrappers pick a unique name).
    let usedNames        = parent!.children.filter{ $0 !== self }.map{ $0.identifier.stringByDeletingPathExtension }
    let newExtension     = newIdentifier.pathExtension
    let uniqueIdentifier = (newIdentifier.pathExtension.isEmpty)
          ? nextName(newIdentifier, usedNames)
          : nextName(newIdentifier.stringByDeletingPathExtension, usedNames).stringByAppendingPathExtension(newExtension)

      // NB: If we try to set the `preferredFileName` without removing the file wrapper from its parent first, we trigger
      //     a fast enumeration exception — although the docs suggest that the file wrapper class would handle that
      //     automatically. Moreover, we disambiguate the name ourselves, because a duplicate during adding also leads to
      //     an exception.
    if let wrapper = fileWrapper {

      parentFileWrapper.removeFileWrapper(wrapper)
      wrapper.preferredFilename = uniqueIdentifier!
      parentFileWrapper.addFileWrapper(wrapper)

    }
    if let playground = playground { playground.renameToTrackModule(uniqueIdentifier!, parent: parentFileWrapper) }

    identifier = uniqueIdentifier!
    return (newIdentifier == uniqueIdentifier!) ? nil : uniqueIdentifier
  }
}


// MARK: -
// MARK: QLPreviewItem protocol methods

extension ProjectItem: QLPreviewItem {

  public var previewItemURL: NSURL! { get { return URL } }
}
