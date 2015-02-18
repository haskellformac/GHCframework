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


/// The toplevel groups
///
/// NB: The raw value determines the position of a group in the outline view.
///
public enum ProjectGroupCategory: Int {
  case Package = 0, Executable, ExtraSource, Data

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

/// Each instance of this class represents one entity within the project.
///
/// Invariants not captured in the type structure:
///
///   * An item has *either* `children` *or* a `fileContent`. (We return "" as the file content for items with children
///     and ignore any updates.)
///   * If an item with `children` is associated with a file wrapper, it is always a directory file wrapper.
///   * If an item with a `fileContent` is associated with a file wrapper, it is always a regular file wrapper.
///
public final class ProjectItem: NSObject {

  let category:   ProjectItemCategory     // The flavour of the item and flavour-specific information
  var identifier: String                  // Identifier that is used to display the item — mutable to rename

  /// The project view model that this item belongs to. (Weak reference as this item is owned by the model.)
  ///
  weak var viewModel: HFMProjectViewModel!

  weak var parent:      ProjectItem?      // Current parent item unless `catagory` is `.Group`
       var children:    [ProjectItem]     // All items that comprise the content of the current one
       var fileWrapper: NSFileWrapper?    // Associated file wrapper for items represented in the file system

  private var dirtyFileContents: String?  // Non-nil if the associated file has been changed and still needs to be saved.
  dynamic var fileContents:      String { // KVO complaint access to the file contents associated with this item
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
                                  parent.viewModel.extraSrcFiles as [String: AnyObject],
                                  false,
                                  parent.viewModel,
                                  parent)

    // Data group: depending on whether 'data-dir:' is defined, we either...
  case .Data:
    if let dataDir = parent.viewModel.dataDir  {   // ...start with a file group or...

      // File group: 1 child: 'data-dir:'
      let dataDirFileWrapper = fileWrapperForFileGroup(parent.viewModel.fileWrapper, dataDir)
      let makeChildren       = { newParent in childrenFromDictionary(dataDirFileWrapper,
                                                                     parent.viewModel.dataFiles as [String: AnyObject],
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
                                    parent.viewModel.dataFiles as [String: AnyObject],
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
                                                                     parent.viewModel.modules as [String: AnyObject],
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
                                  parent.viewModel.modules as [String: AnyObject],
                                  true,
                                  parent.viewModel,
                                  parent)
  }
}


// MARK: -
// MARK: Auxiliary functions traversing file wrapper trees

/// Compute an array of child items from their path dictionary and file wrapper structure.
///
func childrenFromDictionary(rootFileWrapper: NSFileWrapper,
                            pathDict:        [String: AnyObject],
                            asSourceModules: Bool,
                            viewModel:       HFMProjectViewModel,
                            parent:          ProjectItem        ) -> [ProjectItem]
{
  let haskellFileExtension    = HFMProjectViewModel.haskellFileExtension()
  var children: [ProjectItem] = []

  for (fname, subdict) in pathDict {

    let fnameDict = subdict as [String: AnyObject]

    var identifier = fname
    let isFolder   = fnameDict.count > 0
    let isMainFile = !isFolder && fname.pathExtension == haskellFileExtension
    var playground: ProjectViewModelPlayground?

    if (asSourceModules && !isFolder) {     // We have got a Haskell source file

        // Only the main file has an extension in the model => add it to all other sources
      if !isMainFile { identifier = fname.stringByAppendingPathExtension(haskellFileExtension)! }

        // Get the associated playground
      if let playgroundFilename = ProjectViewModelPlayground.implicitPlaygroundFilename(fname) {
        if let playgroundFileWrapper = rootFileWrapper.fileWrappers[playgroundFilename] as? NSFileWrapper {

          playground = ProjectViewModelPlayground(fileWrapper: playgroundFileWrapper, model: viewModel)

        } else {

          playground = ProjectViewModelPlayground(identifier: fname, model: viewModel)

        }
      }
      let fileDetails: ProjectFileCategory = (asSourceModules && playground != nil)
                                               ? .Haskell(isMainFile: isMainFile, playground: playground!)
                                               : .Other
      let category: ProjectItemCategory    = isFolder ? .Folder
                                                      : .File(details: fileDetails)
      if let fileWrapper = rootFileWrapper.fileWrappers[fname] as? NSFileWrapper {

        let makeChildren = { newParent in
                               childrenFromDictionary(fileWrapper, fnameDict, asSourceModules, viewModel, newParent)}
        children.append(ProjectItem(itemCategory: category,
                                    identifier: fname,
                                    viewModel: viewModel,
                                    parent: parent,
                                    fileWrapper: fileWrapper,
                                    children: makeChildren))

      } else {

          // This shouldn't happen...
        children.append(ProjectItem(itemCategory: category,
                                    identifier: fname,
                                    viewModel: viewModel,
                                    parent: parent,
                                    fileWrapper: nil,
                                    children: const([])))
      }
    }
  }
  return children
}

/// Given a wildcard pattern for a file group and the file wrapper at which it is rooted, return the file wrapper that
/// contains the files in the file group.
///
// FIXME: add tests for this function
public func fileWrapperForFileGroup(rootFileWrapper: NSFileWrapper, wildcardPath: String) -> NSFileWrapper {

  let wildcardChars               = NSCharacterSet(charactersInString: "*")
  var fileWrapper: NSFileWrapper? = rootFileWrapper

    // Chase the path components down the directory hierarchy.
  for component in wildcardPath.pathComponents {

    if let wrapper = fileWrapper {

        // File groups that include wildcards are always filenames (not just directory names) and must disregard the
        // filename wildcard component to determine their directory wrapper.
      let wildcardRange = component.rangeOfCharacterFromSet(wildcardChars)
      if wildcardRange == nil { break }

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

  /// Returns the item's file path *relative* to the document root *if* the item is associated with a regular file.
  ///
  var filePath: String? { get {
    if !regularFile { return nil }

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
      return (parent.children as NSArray).indexOfObject(self)
    } else {
      return groupCategory?.rawValue ?? 0
    } }
  }
}

// Queries to quickly test categories.
//
extension ProjectItem {

  var isGroup: Bool { get { return groupCategory != nil } }

  /// Determine the group category in which the item is located in the view model.
  ///
  var groupCategory: ProjectGroupCategory? { get {
    switch category {
    case .Group(category: let category): return category
    default:                             if let parent = parent { return parent.groupCategory } else { return nil }
    } } }

  /// Whether the item is located in the executable group.
  ///
  var isInExecutableCategory: Bool { get {
    switch groupCategory {
    case .Some(.Executable): return true
    default:                 return false
    }
    } }

  /// Whether the item is located in the extra source group.
  ///
  var isInExtraSourceCategory: Bool { get {
    switch groupCategory {
    case .Some(.ExtraSource): return true
    default:                  return false
    }
    } }

  /// Whether the item is located in the data group.
  ///
  var isInDataCategory: Bool { get {
    switch groupCategory {
    case .Some(.Data): return true
    default:           return false
    }
    } }

  var isPackageGroup: Bool { get {
    switch category {
    case .Group(category: .Package): return true
    default:                         return false
    } } }

  var isExtraSourceGroup: Bool { get {
    switch category {
    case .Group(category: .ExtraSource): return true
    default:                             return false
    } } }

  var isDataGroup: Bool { get {
    switch category {
    case .Group(category: .Data): return true
    default:                      return false
    } } }

  var isPackage: Bool { get {
    switch category {
    case .Package: return true
    default:       return false
    } } }

  var isExecutable: Bool { get {
    switch category {
    case .Executable: return true
    default:          return false
    } } }

  var isFileGroup: Bool { get {
    switch category {
    case .FileGroup: return true
    default:         return false
    } } }

  var isFolder: Bool { get {
    switch category {
    case .Folder: return true
    default:      return false
    } } }

  var isFile: Bool { get {
    switch category {
    case .File(details: _): return true
    default:                return false
    } } }

  var isMainFile: Bool { get {
    switch category {
    case .File(details: .Haskell(isMainFile: let isMainFile, playground: _)) where isMainFile: return true
    default:                                                                                   return false
    } } }

  var playground: ProjectViewModelPlayground? { get {
    switch category {
    case .File(details: .Haskell(isMainFile: let _, playground: let playground)): return playground
    default:                                                                      return nil
    } } }

  var isEmptyFolder: Bool { get {
    switch category {
    case .Folder, .FileGroup: return children.map{$0.isEmptyFolder}.reduce(true){$0 && $1}
    default: return false
    } } }

  var isDirectory: Bool { get {               // I.e., may have files as children
    return isExtraSourceGroup || isDataGroup || isExecutable || isFolder || isFileGroup
    } }

  /// Is the main file among the current item's subitems.
  ///
  var containsMainFile: Bool { get {
    return children.map{ $0.isMainFile || $0.containsMainFile }.reduce(false){$0 || $1}
    } }
}


// MARK: -
// MARK: Edits

extension ProjectItem {

  /// Create a new empty file for regular file items whose file wrapper is `nil`. (These are files that are listed in
  /// the Cabal file, but do not exist in the file system.)
  ///
  func touch() {
    if !regularFile { return }

      // Be careful to no overwrite any existing file contents.
    if fileWrapper == nil && dirtyFileContents == nil { dirtyFileContents = fileContents }
  }

  /// Copy the given file into the current item (which must be a folder or group) at the given child index.
  ///
  /// NB: Overwrites any existing file at the destination without further checking.
  ///
  func copyFileAtURL(url: NSURL, toIndex: Int, error: NSErrorPointer) -> Bool {
    if !isDirectory || fileWrapper != nil { return false }
    if children.count < index { return false }


      // Attempt to copy the file. (We attempt to remove any file at the target's name first; if there is a name clash,
      // the caller will have made sure that overwritting the original file is ok.)
    let fileManager = NSFileManager.defaultManager()
    if let identifier  = url.lastPathComponent {
      if let destination = URL?.URLByAppendingPathComponent(identifier) {

        if fileManager.fileExistsAtPath(destination.path!) &&
          !fileManager.trashItemAtURL(destination, resultingItemURL: nil, error: error) { return false }
        if !fileManager.copyItemAtURL(url, toURL: destination, error: error) { return false }

      } else { return false }

        // Update the current item's file wrapper (to pick up the new file).
      if !(fileWrapper!.readFromURL(URL!, options: NSFileWrapperReadingOptions(0), error: error)) { return false }

        // Grab the file wrapper of the new item, create the item, and write the updated Cabal file.
      if let newItemFileWrapper = fileWrapper!.fileWrappers[identifier] as? NSFileWrapper {

          // FIXME: generalise to also cover Haskell files
        let newChild = ProjectItem(itemCategory: .File(details: .Other),
                                   identifier: identifier,
                                   viewModel: viewModel,
                                   parent: self,
                                   fileWrapper: newItemFileWrapper,
                                   children: const([]))
        children.insert(newChild, atIndex:index)

        return viewModel.writeCabalFileWithError(error)

      } else {

        error.memory = NSError(domain: "HFM Internal Error", code: -1, userInfo: nil)
        return false
        
      }
    } else { return false }
  }

  /// Create a new item for a Haskell source file as a child of the current item at the given child index.
  ///
  func newHaskellSourceAtIndex(index: Int) -> Bool {
    if !isDirectory || fileWrapper != nil || (isGroup && !isExtraSourceGroup) { return false }
    if children.count < index { return false }

    let hsExtension = HFMProjectViewModel.haskellFileExtension()
    let usedNames   = children.map{$0.identifier.stringByDeletingPathExtension}
    let identifier  = nextName("NewSource", usedNames).stringByAppendingPathExtension(hsExtension)!

    let newPlayground = ProjectViewModelPlayground(identifier: identifier, model: viewModel)
    let newChild      = ProjectItem(itemCategory: .File(details: .Other),
                                    identifier: identifier,
                                    viewModel: viewModel,
                                    parent: self,
                                    fileWrapper: nil,
                                    children: const([]))
    children.insert(newChild, atIndex: index)
    newChild.fileContents = ""                     // This marks the items as dirty.
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
    if !(children as NSArray).containsObject(childItem) { return false }
    children.filter{ $0 !== childItem }

    // Remove the associated file wrapper.
    if let wrapper = fileWrapper {
      wrapper.removeFileWrapper(childItem.fileWrapper!)
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
