//
//  ContextController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 12/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  The context controller is in charge of the editing and playground context. The context changes by the user browsing
//  into different parts of the project. The context determines the content of the current editor session and of the
//  current playground (which in turns determines the GHC session context).

import Cocoa


// MARK: -
// MARK: Constants

/// Editor NIB file names
///
private let kPackageHeaderEditor = "PackageHeaderEditor"
private let kTextEditor          = "TextEditor"
private let kPlayground          = "Playground"

/// A dictionary associating file extensions with the editor used to edit files of that type. Editors are identified
/// be the name of their NIB file.
///
private let editors = ["cabal": kPackageHeaderEditor,
                       "hs":    kTextEditor,
                       "txt":   kTextEditor,
                       "md":    kTextEditor]
                       // FIXME: we need to consolidate file suffixes

/// Specification of all possible context configurations.
///
private enum Configuration {
  case NoEditor
  case PackageHeaderEditor(HFMHeaderEditorController)
  case TextEditor(TextEditorController)
  case HaskellEditor(TextEditorController, PlaygroundController)
}

// FIXME: We could get rid of the 'NSObject' superclass if we rewrite 'HFMWindowController' in Swift. Or we could make this a proper subclass of 'NSController'.
class ContextController : NSObject {

  /// The project in which this context is located.
  ///
  private let project: HFMProject

  /// The item that determines the context.
  ///
  private var item: HFMProjectViewModelItem?

  /// The current context configuration.
  ///
  private var config = Configuration.NoEditor


  // MARK: -
  // MARK: Initialisation

  init(project: HFMProject) {
    self.project = project
  }


  // MARK: -
  // MARK: Change notification

  /// Switch the context over to the specified item.
  ///
  /// NB: The awkward signature is to return two values to Objective-C. We expect 'editor.memory' and 'playground.memory'
  ///     to be 'nil' on entry to this function.
  func selectItem(item:           HFMProjectViewModelItem,
                  returningEditor
                  editor:         AutoreleasingUnsafeMutablePointer<NSViewController>,
                  playground:     AutoreleasingUnsafeMutablePointer<PlaygroundController>) {
    self.item   = item
    self.config = .NoEditor

    let fileName = item.fileName()
    if fileName == nil { return }
    if !item.fileWrapper.regularFile { return }

    let fileURL       = project.fileURL.URLByAppendingPathComponent(fileName)
    let fileExtension = fileName.pathExtension

      // Check that the file is still there and force reading its contents. (We'll need it in a sec.)
    var error: NSError?
    if fileURL == nil { return }
    if !item.fileWrapper.readFromURL(fileURL, options: .Immediate, error: &error) {
      NSLog("%s: re-reading file wrapper from %@ failed: %@", __FUNCTION__, fileURL,
        error == nil ? "unknown reason" : error!)
      return
    }

      // Select a suitable editor, and try to load the editor and maybe also playground.
    let nibName = editors[fileExtension]
    if let nibName = nibName {

      switch nibName {

      case kPackageHeaderEditor:
        let editorController = HFMHeaderEditorController(nibName: nibName,
                                                         bundle: nil,
                                                         projectViewModel: project.projectModel,
                                                         projectURL: fileURL) as HFMHeaderEditorController?
        if let editorController = editorController {
          config        = .PackageHeaderEditor(editorController)
          editor.memory = editorController
        } else {
          config = .NoEditor
        }

      case kTextEditor:
        let editorController = TextEditorController(nibName: nibName,
                                                    bundle: nil,
                                                    projectViewModelItem: item,
                                                    fileURL: fileURL)
        editor.memory = editorController
        if fileExtension == HFMProjectViewModel.haskellFileExtension() {

          let playgroundController = PlaygroundController(nibName: kPlayground, bundle: nil, projectViewModelItem: item)
          config            = .HaskellEditor(editorController, playgroundController)
          playground.memory = playgroundController

        } else {
          config = .TextEditor(editorController)
        }

      default:
        config = .NoEditor
      }
    }
  }
}

extension ContextController {

  override func commitEditing() -> Bool {
    switch config {
    case .NoEditor:
      break
    case .PackageHeaderEditor(let headerEditor):
      return headerEditor.commitEditing()
    case .TextEditor(let textEditor):
      return textEditor.commitEditing()
    case .HaskellEditor(let textEditor, let playground):
      return textEditor.commitEditing() && playground.commitEditing()
    }
    return true
  }
}
