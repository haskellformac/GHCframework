//
//  ProjectItemGroups.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Collects the toplevel item groups —corresponding to Cabal catgeories— and used to structure the outline view.

import Foundation


// FIXME: Needs to be a class and subtype of NSObject for now — at least until HFMProjectViewModel is in Swift.
public final class ProjectItemGroups: NSObject {

  public let groups: [ProjectGroupCategory: ProjectItem]
  public var count:  Int {
    get { return groups.count }
  }

  init(groups: [ProjectGroupCategory: ProjectItem]) {
    self.groups = groups
  }

  convenience init(viewModel: HFMProjectViewModel) {
    let groups: [ProjectGroupCategory: ProjectItem] =
          [ .Package:     ProjectItem(groupCategory: .Package,     viewModel: viewModel)
          , .Executable:  ProjectItem(groupCategory: .Executable,  viewModel: viewModel)
          , .ExtraSource: ProjectItem(groupCategory: .ExtraSource, viewModel: viewModel)
          , .Data:        ProjectItem(groupCategory: .Data,        viewModel: viewModel)
          ]
    self.init(groups: groups)
  }
}

extension ProjectItemGroups {

  /// Get the group item corresponding to the given index from the source view.
  ///
  func groupItemByIndex(index: Int) -> ProjectItem? {
    if let category = ProjectGroupCategory(rawValue: index) { return groups[category] } else { return nil }
  }

  /// Flush any dirty items in the given group into the file wrappers structure.
  ///
  func flushGroup(category: ProjectGroupCategory) {
    if let item = groups[category] {

      if let fileWrapper = item.viewModel.fileWrapper { updateFileWrapper(fileWrapper, item) }
      else { NSLog("%@: missing toplevel file wrapper for  %@", __FUNCTION__, category.description) }

    } else { NSLog("%@: missing group item: %@", __FUNCTION__, category.description) }
  }
}


// MARK: -
// MARK: Definitions to make access from Objective-C easier.

extension ProjectItemGroups {

  public var packageGroupItem:     ProjectItem { return groups[.Package]! }
  public var executableGroupItem:  ProjectItem { return groups[.Executable]! }
  public var extraSourceGroupItem: ProjectItem { return groups[.ExtraSource]! }
  public var dataGroupItem:        ProjectItem { return groups[.Data]! }

  var allExceptPackageItem: [ProjectItem] { return [executableGroupItem, extraSourceGroupItem, dataGroupItem] }

  func flushPackageGroup()     { flushGroup(.Package) }
  func flushExecutableGroup()  { flushGroup(.Executable) }
  func flushExtraSourceGroup() { flushGroup(.ExtraSource) }
  func flushDataGroup()        { flushGroup(.Data) }

  func flushAllExceptPackageGoup() { flushExecutableGroup(); flushExtraSourceGroup(); flushDataGroup() }
}


// MARK: -
// MARK: Flushing view model changes to file wrappers

/// For all dirty view model items descending from the given item, update the corresponding file wrapper. Create file
/// wrappers for all items that do not yet have one.
///
/// The provided file wrapper is that of the directory directly containing the given items. The function recursively
/// traverses the item tree.
///
private func updateFileWrapper(parentFileWrapper: NSFileWrapper, item: ProjectItem) {
  switch item.category {

  case .Group(_), .Executable:                                // Virtual container items
    map(item.children){ updateFileWrapper(parentFileWrapper, $0) }

  case .FileGroup, .Folder:                                   // Items representing directories
    if let itemFileWrapper = item.cleanFileWrapperWithParent(parentFileWrapper) {
      map(item.children){ updateFileWrapper(itemFileWrapper, $0) }
    }

  case .Package, .File(_):                                    // Items representing files
    item.cleanFileWrapperWithParent(parentFileWrapper)
    if let playground = item.playground { playground.cleanFileWrapperWithParent(parentFileWrapper) }

    if item.children.count > 0 { NSLog("%@: file item has got %@ children", __FUNCTION__, item.children.count) }
  }
}
