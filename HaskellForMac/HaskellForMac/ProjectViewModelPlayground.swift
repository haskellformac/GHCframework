//
//  ProjectViewModelPlayground.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  View model representation of playgrounds.
//
//  The format of Haskell playground files is, for the moment, very simple:
//
//  * They are text files, with the first line being "-- Haskell Playground 1.0".
//
//  * After this, we have the verbatim playground source — i.e., a sequence of GHCi statements and import declarations,
//    where every line that doesn't have white space in column 1, begins a new statement.
//
//  The initial playground version line is present in the external playground representation, but it is not part of the
//  HfM internal playground model representation.

import Foundation


   // FIXME: subclass of NSObject, so we can '-alloc' it from ObjC, for the time being...
class ProjectViewModelPlayground: NSObject {

  /// The project view model that this playground belongs to.
  ///
  /// Weak reference as the playground is owned by the model.
  ///
  private weak var model: HFMProjectViewModel!     // Never nil as the model goes before the playground

  /// The file wrapper of the persistent version of this playground.
  ///
  /// NB: We ensure that this file wrapper always has a preferred filename set.
  ///
  private var fileWrapper: NSFileWrapper

  /// The view model data of the playground.
  ///
  var string: NSString {
    get {
      if let data = fileWrapper.regularFileContents {
        return NSString(data: data, encoding: NSUTF8StringEncoding) ?? ""
      } else { return "" }
    }

    set(newString) {
      let preferredFilename = fileWrapper.preferredFilename ?? ".Unknown.hsplay"
      fileWrapper = NSFileWrapper(regularFileWithContents: newString.dataUsingEncoding(NSUTF8StringEncoding)!)
      fileWrapper.preferredFilename = preferredFilename
    }
  }

//  var attributeString: NSAttributedString {
//    get { return NSAttributedString(string: string) }
//    set { string = newValue.string }
//  }

  /// Compute the implicit playground filename from the view model identifier.
  ///
  class func implicitPlaygroundFilename(identifier: String) -> String? {
    if identifier.pathExtension != HFMProjectViewModel.haskellFileExtension() { return nil }

    let playgroundExtension = HFMProjectViewModel.haskellPlaygroundFileExtension()
    if let basename = identifier.stringByDeletingPathExtension.stringByAppendingPathExtension(playgroundExtension) {
      return "." + basename
    } else { return nil }
  }

  // MARK: -
  // MARK: Initialisers

  /// Fails if the identifier doesn't refer to a Haskell file.
  ///
  init?(identifier: String, model: HFMProjectViewModel) {
    self.model       = model
    self.fileWrapper = NSFileWrapper(regularFileWithContents: NSData())
    super.init()

    if let filename = ProjectViewModelPlayground.implicitPlaygroundFilename(identifier) {

      self.fileWrapper.preferredFilename = filename

    } else { return nil }
  }

  init?(fileWrapper: NSFileWrapper, model: HFMProjectViewModel) {
    self.model       = model
    self.fileWrapper = fileWrapper
    super.init()
    if !fileWrapper.regularFile { return nil }
  }
}
