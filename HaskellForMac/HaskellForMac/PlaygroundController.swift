//
//  PlaygroundController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/07/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  A single playground instance is always associated with one context, usually a Haskell module. It never changes its
//  context. A new context implies the creation of a new playground.

import Cocoa
import SpriteKit
import GHCKit
import HaskellSpriteKit


private let kPlaygroundSource = "<playground>"

class PlaygroundController: NSViewController {

  // Views in 'Playground.xib'
  //
  @IBOutlet private weak var splitView:        StyledSplitView!
  @IBOutlet private weak var codeScrollView:   SynchroScrollView!
  @IBOutlet private weak var resultScrollView: SynchroScrollView!
  @IBOutlet private      var codeTextView:     CodeView!
  @IBOutlet private weak var resultTableView:  NSTableView!

  /// The playground model managed by this controller.
  ///
  private let projectViewModelPlayground: ProjectViewModelPlayground

  /// We need to keep the code storage delegate alive as the delegate reference from `NSTextStorage` is unowned.
  ///
  private var codeStorageDelegate: CodeStorageDelegate!

  /// Meta data keeping track of playground commands.
  ///
  private var commands: PlaygroundCommands!

  /// We need to keep the result storage alive as the data source reference from `NSTableView` is weak.
  ///
  private var resultStorage: PlaygroundResultStorage!

  /// The GHC session associated with this playground.
  ///
  /// NB: We need to use an implicit optional as we can only initialise after calling `super.init` in `init` (as we need
  ///     to capture `self`).
  private var haskellSession: HaskellSession!

  private let fontHeight: CGFloat = {
    let x = NSAttributedString(string: "X", attributes: [NSFontAttributeName: NSFont(name: "Menlo-Regular", size:13)!])
    return x.size.height
  }()

  /// Bin to collect issues for this playground
  ///
  private var issues = IssuesForFile(file: kPlaygroundSource, issues: [:])

  // Objects from the results popover nib.
  //
  @IBOutlet private      var popover:           NSPopover?               // referenced to retain
  @IBOutlet private      var popoverController: NSViewController!        // referenced to retain
  @IBOutlet private weak var popoverTextField:  NSTextField!             // This is where the textual result goes.

  @IBOutlet private      var resultPopover:           NSPopover?         // referenced to retain
  @IBOutlet private      var resultPopoverController: NSViewController!  // referenced to retain
  @IBOutlet private weak var resultPopoverView:       NSView!            // This is where the graphical result goes.

  //MARK: -
  //MARK: Initialisation and deinitialisation

  init?(
    nibName:                         String!,
    bundle:                          NSBundle!,
    projectViewModelPlayground:      ProjectViewModelPlayground,
    diagnosticsHandler:              Issue -> Void,
    interactiveWorkingDirectory cwd: String)
  {
    self.projectViewModelPlayground = projectViewModelPlayground
    
      // Call the designated initialiser.
    super.init(nibName: nibName, bundle: bundle)

      // Launch a GHC session for this playground.
    haskellSession = HaskellSession(diagnosticsHandler: processIssue(diagnosticsHandler),
                                    interactiveWorkingDirectory: cwd)
  }

  required init?(coder: NSCoder) {
    // just to keep the compiler happy...
    self.projectViewModelPlayground = ProjectViewModelPlayground(identifier: "", model: HFMProjectViewModel())!
    haskellSession = HaskellSession(diagnosticsHandler: {severity, filename, line, column, lines, endColumn, message in },
                                    interactiveWorkingDirectory: nil)
    super.init(coder: coder)
  }

  deinit {
    resultScrollView.stopSynchronising()
    codeScrollView.stopSynchronising()
  }

  override func awakeFromNib() {

      // Do not re-initialise when a nib is loaded with objects that we are the owner/delegate of — e.g., the popover view.
    if resultStorage != nil { return }

      // Synchronise the scroll views.
    codeScrollView.startSynchronisedScrollView(resultScrollView)
    resultScrollView.startSynchronisedScrollView(codeScrollView)

      // Set up the gutter.
    codeScrollView.hasVerticalRuler = true
    codeScrollView.rulersVisible    = true

      // The size of the playground code view is fixed. We want it to be rigid.
    codeTextView.horizontallyResizable = true

      // Set up for code editing (not prose).
    codeTextView.automaticDashSubstitutionEnabled   = false
    codeTextView.automaticDataDetectionEnabled      = false
    codeTextView.automaticLinkDetectionEnabled      = false
    codeTextView.automaticQuoteSubstitutionEnabled  = false
    codeTextView.automaticSpellingCorrectionEnabled = false
    codeTextView.automaticTextReplacementEnabled    = false

      // FIXME: How can we do that in a locale-independent way.
    var contextMenu = NSTextView.defaultMenu()
    if let item = contextMenu?.itemWithTitle("Spelling and Grammar") { contextMenu?.removeItem(item) }
    if let item = contextMenu?.itemWithTitle("Substitutions")        { contextMenu?.removeItem(item) }
    if let item = contextMenu?.itemWithTitle("Layout Orientation")   { contextMenu?.removeItem(item) }
    codeTextView.menu = contextMenu

      // Set up the delegate for the text storage.
    if let textStorage = codeTextView.layoutManager?.textStorage {
      codeStorageDelegate  = CodeStorageDelegate(textStorage: textStorage)
      textStorage.delegate = codeStorageDelegate
      codeStorageDelegate.loadTriggers.observeWithContext(self,
                                                          observer: curry{ controller, _ in
                                                                           controller.executePendingCommands() })
    }

      // Set up the delegate and data source for the result view.
    let reloadDataForRow: Int -> () = { [unowned self] (row: Int) in
      let rowSet    = NSIndexSet(index: row)
      let columnSet = NSIndexSet(indexesInRange: NSRange(location: 0,
                                                         length: self.resultTableView.tableColumns.count))
      self.resultTableView.reloadDataForRowIndexes(rowSet, columnIndexes: columnSet)
    }
    resultTableView.setDelegate(self)
    resultStorage = PlaygroundResultStorage(redisplay: resultTableView.reloadData, redisplayRow: reloadDataForRow)
    resultTableView.setDataSource(resultStorage)

      // Get the initial code view contents and enable highlighting.
    codeTextView.string = projectViewModelPlayground.string
    codeTextView.enableHighlighting(tokeniseHaskell(kPlaygroundSource))
    ThemesController.sharedThemesController().reportThemeInformation(self,
      fontChangeNotification: curry{ obj, font in return },
      themeChangeNotification: curry{ $0.setResultTableViewBackgroundAndDividerColour($1) })

      // Set up playground commands tracking. This needs to happen after setting the initial code view contents, so that
      // the initial command parsing has access to the initial playground contents.
    if let textStorage = codeTextView.layoutManager?.textStorage {
      commands = PlaygroundCommands(codeStorage: textStorage)
    }
  }


  // MARK: -
  // MARK: Code loading and execution

  /// Load a new version of the context module asynchronously. The completion handler is being invoked after loading
  /// has finished and it indicates whether loading was successful. If loading was successful, the associated playground
  /// is also executed asynchronously. 
  ///
  /// NB: The completion handler runs right after module loading; it is independent of playground loading. The
  ///     completion handler runs on a *concurrent* queue.
  ///
  /// Diagnostics are delivered asynchronously.
  ///
  func asyncLoadModule(moduleText: String!,
                       file: String,
                       importPaths: [String],
                       alsoLoadPlayground: Bool,
                       completionHandler handler: Bool -> Void)
  {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)){

      let success = self.haskellSession.loadModuleFromString(moduleText, file: file, importPaths: importPaths)
      handler(success)

      self.commands.setAllCommandsPending()
      if alsoLoadPlayground && success { dispatch_async(dispatch_get_main_queue()){ self.executePendingCommands() } }
    }
  }

  /// Execute the given command asynchronously. The completion handler is being invoked after execution has finished.
  ///
  /// NB: The completion handler runs on a *concurrent* queue.
  ///
  /// Diagnostics are delivered asynchronously.
  ///
  func asyncExecuteCommand(command: PlaygroundCommands.Command,
                           completionHandler handler: (AnyObject, [String]) -> Void) {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)){

      let (evalResult: AnyObject, evalTypes) = self.haskellSession.evalExprFromString(command.text,
                                                                                      source: kPlaygroundSource,
                                                                                      line: command.lines.startIndex)
      handler(evalResult, evalTypes)
    }
  }


  // MARK: -
  // MARK: Processing diagnostics

  /// Process an asynchronously delivered diagnostics message.
  ///
  private func processIssue(contextDiagnosticsHandler: Issue -> Void) -> DiagnosticsHandler {
    return {[weak self] severity, filename, line, column, lines, endColumn, message
    in
    let issue = Issue(severity: severity,
                      filename: filename,
                      line: line,
                      column: column,
                      lines: lines,
                      endColumn: endColumn,
                      message: message)
    if filename == kPlaygroundSource {
      self?.issues = addIssueForFile(issue, self!.issues)
    } else {
      contextDiagnosticsHandler(issue)
    }}
  }


  // MARK: -
  // MARK: Playground execution

  /// If there are any commands waiting for execution, execute them and their dependants.
  ///
  private func executePendingCommands() {

      // Do not actually execute commands if the playground is hidden.
    if let superview = view.superview { if superview.hidden { return } }
    else { return }

    let gutter = codeScrollView.verticalRulerView as? TextGutterView

    if let command = commands.nextPendingCommand {              // There are commands that need to be executed...

        // Invalidate old issues, marks pending results as stale, and anounce that the playground gets loaded.
      gutter?.updateIssues(.IssuesPending)
      resultStorage.invalidateFrom(command.index)
      codeStorageDelegate.status.value = .LastLoading(NSDate())

        // Discard all old issues.
      issues = IssuesForFile(file: issues.file, issues: [:])

        // Run the command.
      asyncExecuteCommand(command){ (evalResult, evalTypes) in

          // Interacting with playground commands and results views must happen on the *main* thread!
        dispatch_async(dispatch_get_main_queue()){

          if self.commands.markAsCompleted(command) {
            self.reportResultAtIndex(command.index, result: evalResult, withTypes: evalTypes)
          }
          self.executePendingCommands()
        }
      }

    } else {                                                    // All results are up to date; nothing to be executed...

        // Make sure any overhanging old items in the results storage are discarded.
      resultStorage.pruneAt(commands.count)

        // Display any diagnostics in the gutter.
      if issues.issues.isEmpty {
        gutter?.updateIssues(.NoIssues)
      } else {
        gutter?.updateIssues(.Issues(issues))
      }

    }
  }

  /// Report an evaluation result by entering it into the result storage.
  ///
  private func reportResultAtIndex(index: Int, result: AnyObject, withTypes evalTypes: [String]) {
    if let resultScene = spriteKitView(result) {

      // Graphical result with custom presentation view.
      resultStorage.reportResult(.SKSceneResult(scene: resultScene), type: ", ".join(evalTypes), atCommandIndex: index)

    } else if let resultImage = result as? NSImage {

      // The result is just a static image.
      resultStorage.reportResult(.ImageResult(image: resultImage), type: ", ".join(evalTypes), atCommandIndex: index)

    } else if let resultText = result as? String {

      // The result is just a string.
      resultStorage.reportResult(.StringResult(string: resultText), type: ", ".join(evalTypes), atCommandIndex: index)

    } else {

      // No idea what this result is.
      resultStorage.reportResult(.StringResult(string: "«unknown type of result»"),
                                 type: ", ".join(evalTypes),
                                 atCommandIndex: index)
    }
  }
}


// MARK: -
// MARK: Syntax highlighting support

extension PlaygroundController {

  func setResultTableViewBackgroundAndDividerColour(theme: Theme) {
    resultTableView.backgroundColor = gutterColour(theme)
    splitView.customDividerColor    = dividerColour(theme)
    splitView.needsDisplay          = true      // ...to redraw the divider
    resultTableView.reloadData()
  }

  func tokeniseHaskell(file: String) -> HighlightingTokeniser {
    return { (line, column, text) in
      map(self.haskellSession.tokeniseHaskell(text, file: file, line: line, column: column)){ token in
        HighlightingToken(ghcToken: token) }
    }
  }

}


// MARK: -
// MARK: NSTextDelegate protocol methods (for the code view)

extension PlaygroundController: NSTextDelegate {

  func textDidChange(notification: NSNotification) {

    projectViewModelPlayground.string = codeTextView.string ?? ""
    commands.scanCodeStorage()
  }
}


// MARK: -
// MARK: NSTextViewDelegate protocol methods (for the code view)

extension PlaygroundController: NSTextViewDelegate {

  // Currently, none of the delegate methods are needed.
}


// MARK: -
// MARK: First responder

extension PlaygroundController {

  func isCodeViewFirstResponder() -> Bool {
    return codeTextView.window?.firstResponder === codeTextView
  }

  func makeCodeViewFirstResponder () {
    codeTextView.window?.makeFirstResponder(codeTextView)
  }
}


// MARK: -
// MARK: NSTableViewDelegate protocol methods

extension PlaygroundController: NSTableViewDelegate {

  func tableView(tableView: NSTableView, viewForTableColumn _column: NSTableColumn?, row: Int) -> NSView? {

    if let result = resultStorage.queryResult(row) {
      if let cell = tableView.makeViewWithIdentifier(kResultCell, owner: self) as? ResultCellView {

        cell.configureResult(result)
        return cell

      } else { return nil }
    } else { return nil }
  }

  func tableView(tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
    if let result = resultStorage.queryResult(row) {
      return fontHeight //result.height
    } else { return 0 }
  }

  func tableViewSelectionDidChange(notification: NSNotification) {
    let tableView = (notification.object as? NSTableView)!
    let row       = tableView.selectedRow
    if row == -1 { return }  // no row selected

    if let result = resultStorage.queryResult(row) {

        // Get the frame of the table view cell whose contents is to be displayed in the popover.
      let rowView  = resultTableView.rowViewAtRow(row, makeIfNecessary: false) as? NSView
      if let frame = rowView?.frame {

        switch result.value {
        case .SKSceneResult(let scene0):     // result with a SpriteKit scene
          let scene: SKScene = scene0 // FIXME: current version of the Swift compilers needs this to infer the right type
          let bundle = NSBundle.mainBundle()
          if !bundle.loadNibNamed("ResultViewPopover", owner: self, topLevelObjects: nil) {
            NSLog("%@: could not load result popover NIB", __FUNCTION__)
          } else {

              // Ensure the scene size is valid.
            var sceneSize = scene.size
            if !isfinite(sceneSize.width)  || sceneSize.width  < 30 { sceneSize.width  = 30 }
            if !isfinite(sceneSize.height) || sceneSize.height < 30 { sceneSize.height = 30 }
            scene.size = sceneSize

              // Constrain the popover size.
            let resultSize = CGSize(width:  clampExtent(sceneSize.width,  50, 1024),
                                    height: clampExtent(sceneSize.height, 50, 768))
            var resultView = SKView(frame: CGRect(origin: CGPointZero, size: resultSize))
            resultView.autoresizingMask = NSAutoresizingMaskOptions.ViewNotSizable
            resultView.translatesAutoresizingMaskIntoConstraints = true
            resultView.presentScene(scene)

              // Add the SpriteKit view to the popover.
            for view in resultPopoverView.subviews { view.removeFromSuperview() }
            resultPopoverView.frame = CGRect(origin: CGPoint(x: 0, y: 0), size: resultSize)
            resultPopoverView.autoresizingMask = NSAutoresizingMaskOptions.ViewNotSizable
            resultPopoverView.translatesAutoresizingMaskIntoConstraints = true
            resultPopoverView.addSubview(resultView)

              // And present it.
            resultPopover?.delegate    = self
            resultPopover?.contentSize = resultSize
            resultPopover?.behavior    = .Semitransient
            resultPopover?.showRelativeToRect(NSRect(origin: frame.origin, size: CGSize(width: 10, height: 15)),
                                              ofView: resultTableView,
                                              preferredEdge: NSMaxYEdge)
          }

          // FIXME: There is a lot of overlap between this and the scene case => refactor
        case .ImageResult(let image0):
          let image: NSImage = image0 // FIXME: current version of the Swift compilers needs this to infer the right type
          let bundle = NSBundle.mainBundle()
          if !bundle.loadNibNamed("ResultViewPopover", owner: self, topLevelObjects: nil) {
            NSLog("%@: could not load result popover NIB", __FUNCTION__)
          } else {

              // Ensure the image size is valid.
            var imageSize = image.size
            if !isfinite(imageSize.width)  || imageSize.width  < 30 { imageSize.width  = 30 }
            if !isfinite(imageSize.height) || imageSize.height < 30 { imageSize.height = 30 }

              // Constrain the popover size.
            let resultSize = CGSize(width:  clampExtent(imageSize.width,  50, 1024),
                                    height: clampExtent(imageSize.height, 50, 768))
            var resultView = NSImageView(frame: CGRect(origin: CGPointZero, size: resultSize))
            resultView.autoresizingMask = NSAutoresizingMaskOptions.ViewNotSizable
            resultView.translatesAutoresizingMaskIntoConstraints = true
            resultView.image = image

              // Add the image view to the popover.
            for view in resultPopoverView.subviews { view.removeFromSuperview() }
            resultPopoverView.frame = CGRect(origin: CGPoint(x: 0, y: 0), size: resultSize)
            resultPopoverView.autoresizingMask = NSAutoresizingMaskOptions.ViewNotSizable
            resultPopoverView.translatesAutoresizingMaskIntoConstraints = true
            resultPopoverView.addSubview(resultView)

              // And present it.
            resultPopover?.contentSize = resultSize
            resultPopover?.behavior    = .Semitransient
            resultPopover?.showRelativeToRect(NSRect(origin: frame.origin, size: CGSize(width: 10, height: 15)),
                                              ofView: resultTableView,
                                              preferredEdge: NSMaxYEdge)
          }

        case .StringResult(let string0):     // text only result
          let string: String = string0 // FIXME: current version of the Swift compilers needs this to infer the right type
          if !string.isEmpty {

            let bundle = NSBundle.mainBundle()
            if !bundle.loadNibNamed("ResultPopover", owner: self, topLevelObjects: nil) {
              NSLog("%@: could not load popover NIB", __FUNCTION__)
            } else {

                // By default, we go for the width of the window in the NIB and compute the required height given the
                // string we need to display.
              let width = popover?.contentViewController?.view.bounds.size.width ?? 400
              popoverTextField.stringValue             = string
              popoverTextField.preferredMaxLayoutWidth = width
              popoverTextField.sizeToFit()

              let textSize     = popoverTextField.intrinsicContentSize
              let contentWidth = textSize.width < width ? max(textSize.width, 50) : width
              if textSize.width < contentWidth {
                // FIXME: center the content when it is smaller than the popover, but how?
              }
              let contentSize    = NSSize(width: contentWidth, height: textSize.height > 500 ? 500 : textSize.height)
                // FIXME: How can we determine the constants programatically? NSScrollView.frameSizeForContentSize(_:_:_:_:_:)
                //        doesn't seem to work as exepcted.
              let scrollViewSize = CGSize(width: contentSize.width + 4, height: contentSize.height + 4)
              popover?.contentSize = scrollViewSize
              popover?.behavior    = .Semitransient

              popover?.showRelativeToRect(NSRect(origin: frame.origin, size: CGSize(width: 10, height: 15)),
                                          ofView: resultTableView,
                                          preferredEdge: NSMaxYEdge)
            }
          }
        }
      }
    }
    resultTableView.deselectRow(row)    // .. so that selecting again will call this function again
  }
}


// MARK: -
// MARK: NSPopoverDelegate protocol methods


extension PlaygroundController: NSPopoverDelegate {

    // We currently only use this notification to stop the animation in SKView popovers when they are closed.
  func popoverDidClose(notification: NSNotification) {
    for view in resultPopoverView.subviews { (view as? SKView)?.scene?.paused = true }
  }
}