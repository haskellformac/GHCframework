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
class ProjectItemGroups: NSObject {

  let groups: [ProjectGroupCategory: ProjectItem]
  var count:  Int {
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

  func groupItemByIndex(index: Int) -> ProjectItem? {
    if let category = ProjectGroupCategory(rawValue: index) { return groups[category] } else { return nil }
  }
}


// MARK: -
// MARK: Definitions to make access from Objective-C easier.

extension ProjectItemGroups {

  var packageGroupItem:     ProjectItem { get { return groups[.Package]! } }
  var executableGroupItem:  ProjectItem { get { return groups[.Executable]! } }
  var extraSourceGroupItem: ProjectItem { get { return groups[.ExtraSource]! } }
  var dataGroupItem:        ProjectItem { get { return groups[.Data]! } }

  var allExceptPackageItem: [ProjectItem] { get { return [executableGroupItem, extraSourceGroupItem, dataGroupItem] } }
}
