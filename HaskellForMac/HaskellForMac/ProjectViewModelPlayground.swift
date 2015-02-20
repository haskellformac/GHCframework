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
//  * They are text files, with the first line being "-- Haskell Playground 1.0\n".
//
//  * After this, we have the verbatim playground source — i.e., a sequence of GHCi statements and import declarations,
//    where every line that doesn't have white space in column 1, begins a new statement.
//
//  The initial playground version line is present in the external playground representation, but it is not part of the
//  HfM internal playground model representation.

import Foundation


private let playgroundMagic = "-- Haskell Playground 1.0\n"

private func stripPlaygroundMagic(contents: String) -> String {
  if contents.hasPrefix(playgroundMagic) { return contents.substringFromIndex(playgroundMagic.endIndex) }
  else { return "" }
}

   // FIXME: subclass of NSObject, so we can '-alloc' it from ObjC, for the time being...
public class ProjectViewModelPlayground: NSObject {

  /// The project view model that this playground belongs to.
  ///
  /// Weak reference as the playground is owned by the model.
  ///
  private weak var model: HFMProjectViewModel!     // Never nil as the model goes before the playground

  /// The file wrapper of the persistent version of this playground.
  ///
  /// NB: We ensure that this file wrapper always has a preferred filename set.
  ///
  private var theFileWrapper: NSFileWrapper

  var fileWrapper: NSFileWrapper { return theFileWrapper }

  /// The view model data of the playground.
  ///
  var string: NSString {
    get {
      if let data = theFileWrapper.regularFileContents {
        return stripPlaygroundMagic(NSString(data: data, encoding: NSUTF8StringEncoding) ?? "")
      } else { return "" }
    }

    set(newString) {
      let preferredFilename = theFileWrapper.preferredFilename ?? ".Unknown.hsplay"
      let fileContents      = (playgroundMagic + newString).dataUsingEncoding(NSUTF8StringEncoding)!
      theFileWrapper = NSFileWrapper(regularFileWithContents: fileContents)
      theFileWrapper.preferredFilename = preferredFilename
    }
  }

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
    self.model          = model
    self.theFileWrapper = NSFileWrapper(regularFileWithContents: NSData())
    super.init()

    if let filename = ProjectViewModelPlayground.implicitPlaygroundFilename(identifier) {

      self.theFileWrapper.preferredFilename = filename

    } else { return nil }
  }

  init?(fileWrapper: NSFileWrapper, model: HFMProjectViewModel) {
    self.model          = model
    self.theFileWrapper = fileWrapper
    super.init()
    if !fileWrapper.regularFile { return nil }
  }


  // MARK: -
  // MARK: Flushing changes

  /// Ensure that the parent file wrapper contains the latest version of the playground file wrapper.
  ///
  func cleanFileWrapperWithParent(parentFileWrapper: NSFileWrapper) {

    if let oldFileWrapper = parentFileWrapper.fileWrappers[fileWrapper.preferredFilename] as? NSFileWrapper {

        // Swap file wrappers if different; otherwise, just leave the old one in there.
      if oldFileWrapper !== fileWrapper {
        parentFileWrapper.removeFileWrapper(oldFileWrapper)
        parentFileWrapper.addFileWrapper(fileWrapper)
      }

    } else { parentFileWrapper.addFileWrapper(fileWrapper) }
  }
}


// MARK: -
// MARK: File operations

extension ProjectViewModelPlayground {

  /// Remove the associated file wrapper.
  ///
  func remove(moduleName: String, parent: NSFileWrapper) {
    if let filename = ProjectViewModelPlayground.implicitPlaygroundFilename(moduleName) {
      parent.removeFileWrapper(theFileWrapper)
    }
  }

  /// Rename the associated file wrapper such that it matches the Haskell module with the given name in the directory
  /// of the given parent directory file wrapper.
  ///
  func renameToTrackModule(moduleName: String, parent: NSFileWrapper) {
    if let filename = ProjectViewModelPlayground.implicitPlaygroundFilename(moduleName) {

        // NB: According to the docs, we should just be able to change the preferred filename, but that has led to
        //     exceptions in the past; so, we play it safe.
      parent.removeFileWrapper(theFileWrapper)
      theFileWrapper.preferredFilename = filename
      parent.addFileWrapper(theFileWrapper)
    }
  }
}
