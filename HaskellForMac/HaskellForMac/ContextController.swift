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

/// NIB file names
///
private let kPackageHeaderEditor = "PackageHeaderEditor"
private let kTextEditor          = "TextEditor"
private let kPlayground          = "Playground"
private let kQuicklook           = "Quicklook"

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
  case Quicklook(QuicklookController)
  case PackageHeaderEditor(HFMHeaderEditorController)
  case TextEditor(TextEditorController)
  case HaskellEditor(TextEditorController, PlaygroundController)

  func textEditor() -> TextEditorController? {
    switch self {
    case .TextEditor(let editor):           return editor
    case .HaskellEditor(let editor, let _): return editor
    default: return nil
    }
  }
}

/// Context changes that other system components need to react to.
///
enum ContextChange {

    /// The view model item and, hence configuration, determining the context changed.
  case ConfigurationChanged

    /// The Haskell module of the context was successfully loaded into GHC.
  case ModuleLoaded

    /// Loading of the Haskell module of the context failed.
  case ModuleLoadingFailed
}

// FIXME: We could get rid of the 'NSObject' superclass if we rewrite 'HFMWindowController' in Swift. Or we could make this a proper subclass of 'NSController'.
final class ContextController : NSObject {

  /// The project in which this context is located.
  ///
  private let project: HFMProject

  /// The item that determines the context.
  ///
  private var item: ProjectItem?

  /// The current context configuration.
  ///
  private var config: Configuration = .NoEditor
    { didSet { contextChanges.announce(.ConfigurationChanged) } }

  /// Bin to collect issues for the context module.
  ///
  private var issues = IssuesForFile(file: "", issues: [:])

  /// Stream of context changes over the lifetime of this context.
  ///
  let contextChanges: Changes<ContextChange> = Changes()


  // MARK: -
  // MARK: Initialisation

  init(project: HFMProject) {

    self.project = project
    super.init()

      // Observe asynchronous context changes on the main queue.
    contextChanges.asyncObserveWithContext(self, observer: { context in context.contextChange })
  }


  // MARK: -
  // MARK: Change notification

  /// Switch the context over to the specified item.
  ///
  /// NB: The awkward signature is to return two values to Objective-C. We expect 'editor.memory' and 'playground.memory'
  ///     to be 'nil' on entry to this function.
  func selectItem(item:           ProjectItem,
                  returningEditor
                  editor:         AutoreleasingUnsafeMutablePointer<NSViewController>,
                  playground:     AutoreleasingUnsafeMutablePointer<PlaygroundController>)
  {
    self.item   = item
    self.config = .NoEditor

      // Get the filename associated with the new item — this also guarantees that `item` represents a regular file.
    if let filePath = item.filePath {
      if let fileURL = item.URL {

        let fileExtension = filePath.pathExtension

          // Check that the file is still there and force reading its contents unless the item is dirty.
          // (We'll need it in a sec.)
        if let wrapper = item.fileWrapper? {

          var error: NSError?
          if !item.isDirty && !wrapper.readFromURL(fileURL, options: .Immediate, error: &error) {
            NSLog("%s: re-reading file wrapper from %@ failed: %@", __FUNCTION__, fileURL,
                  error == nil ? "unknown reason" : error!)
            return
          }
        }

          // Select a suitable editor, and try to load the editor and maybe also playground.
        let nibName = editors[fileExtension] ?? kQuicklook

        switch nibName {

        case kQuicklook:
          if let quicklookController = QuicklookController(nibName: nibName,
                                                            bundle: nil,
                                              projectViewModelItem: item)
          {
            config        = .Quicklook(quicklookController)
            editor.memory = quicklookController
          } else {
            config = .NoEditor
          }

        case kPackageHeaderEditor:
          if let editorController = HFMHeaderEditorController(nibName: nibName,
                                                               bundle: nil,
                                                     projectViewModel: project.projectModel,
                                                            cabalPath: filePath)
          {
            config        = .PackageHeaderEditor(editorController)
            editor.memory = editorController
          } else {
            config = .NoEditor
          }

        case kTextEditor:
          if let editorController = TextEditorController(nibName: nibName,
                                                          bundle: nil,
                                            projectViewModelItem: item,
                                                      loadModule: { [unowned self] in self.loadContextModule() })
          {
            editor.memory = editorController
            if let viewModelPlayground = item.playground {

              if let playgroundController = PlaygroundController(nibName: kPlayground,
                                                                  bundle: nil,
                                              projectViewModelPlayground: viewModelPlayground,
                                                      diagnosticsHandler: processIssue,
                                             interactiveWorkingDirectory:
                                               project.fileURL!.path!.stringByAppendingPathComponent(item.viewModel?.dataDir ?? ""))
              {

                config            = .HaskellEditor(editorController, playgroundController)
                playground.memory = playgroundController

                  // Initialise the issues bag.
                issues = IssuesForFile(file: fileURL.path!, issues: [:])

                  // Register the tokeniser for syntax highlighting. (The editor depends on the playground for that.)
                editorController.enableHighlighting(playgroundController.tokeniseHaskell(fileURL.path!))

              } else {
                config = .TextEditor(editorController)
                editorController.enableHighlighting(nil)      // Gentle theming
              }
            } else {
              config = .TextEditor(editorController)
              editorController.enableHighlighting(nil)      // Gentle theming
            }
          } else {
            config = .NoEditor
          }

        default:
          config = .NoEditor
        }
      }
    }
  }

  /// Remove the current context.
  ///
  func deselectCurrentItem() {
    self.item   = nil
    self.config = .NoEditor
  }

  /// React to asynchronous context changes.
  ///
  func contextChange(change: ContextChange)
  {
    switch change {
    case .ConfigurationChanged: ()

    case .ModuleLoaded:         // Report any issues (warnings) and notify the editor of the successful loading.
      switch self.config {

      case .HaskellEditor(let editor, let playground):
        if issues.issues.isEmpty { editor.updateIssues(.NoIssues) } else { editor.updateIssues(.Issues(issues)) }

      default: ()
      }

    case .ModuleLoadingFailed:  // Report the issues
      switch self.config {

      case .HaskellEditor(let editor, _):
        if issues.issues.isEmpty { editor.updateIssues(.NoIssues) } else { editor.updateIssues(.Issues(issues)) }

      default: ()
      }
    }
  }

  // MARK: -
  // MARK: Module management

  /// Load the module represented by the current item asynchronously. If loading is successful, the `.ModuleLoaded`
  /// change announcement will also trigger the loading of the associated playground.
  ///
  func loadContextModule() {

    switch config {
    case .HaskellEditor(let editor, let playground):

        // Invalidate old issues and announce that the module gets loaded.
      editor.updateIssues(.IssuesPending)
      issues = IssuesForFile(file: issues.file, issues: [:])
      editor.moduleLoading()

        // Load the module.
      if let item = self.item {
        if let projectPath            = project.fileURL?.path? {
          if let fullFilename         = item.URL?.path {
            let importPaths: [String] =
                  { switch self.project.projectModel.sourceDir {
                    case .None:                return [projectPath]
                    case .Some(let sourceDir): return [projectPath, projectPath.stringByAppendingPathComponent(sourceDir)]
                    }}()
            playground.asyncLoadModule(item.fileContents, file: fullFilename, importPaths: importPaths){ success in
              if success { self.contextChanges.announce(.ModuleLoaded) }
              else { self.contextChanges.announce(.ModuleLoadingFailed) }
            }
          }
        }
      }

    default: ()
    }
  }


  // MARK: -
  // MARK: Diagnostics processing

  // Given an issue for a module in the Cabal project, foward it to the view that is responsible for displaying it.
  //
  private func processIssue(issue: Issue) {

    NSLog("issues: %@", issue.message)

      // FIXME: For the moment, we drop anything that does not apply to the module determining the current context?
    switch config {
    case .HaskellEditor(let editorController, let _):
      issues = addIssueForFile(issue, issues)

    default: break
    }
  }
}


// MARK: -
// MARK: NSEditor informal protocol

extension ContextController {

  // Make sure that all context is being committed.
  //
  override func commitEditing() -> Bool {
    switch config {

    case .NoEditor:
      break

    case .Quicklook(_):
      break

    case .PackageHeaderEditor(let headerEditor):
      return headerEditor.commitEditing()

    case .TextEditor(let textEditor):
      return textEditor.commitEditing()

    case .HaskellEditor(let textEditor, let playground):
      let committed = textEditor.commitEditing() && playground.commitEditing()
      return committed
    }
    return true
  }
}

// MARK: -
// MARK: Forward editor actions that ought to work while a diagnostics popup is shown

extension ContextController {

  func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem) -> Bool {
    return config.textEditor()?.validateUserInterfaceItem(sender) ?? false
  }

  func jumpToNextIssue(sender: AnyObject!) {
    config.textEditor()?.jumpToNextIssue(sender)
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    config.textEditor()?.jumpToPreviousIssue(sender)
  }

}
