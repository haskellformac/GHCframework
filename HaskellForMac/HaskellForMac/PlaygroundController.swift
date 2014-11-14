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
import GHCKit
import HaskellSpriteKit


private let kPlaygroundSource = "<playground>"

class PlaygroundController: NSViewController {

  // Views in 'Playground.xib'
  //
  @IBOutlet private weak var splitView:        NSSplitView!
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

  /// We need to keep the result storage alive as the data source reference from `NSTableView` is weak.
  ///
  private var resultStorage: PlaygroundResultStorage!

  /// The GHC session associated with this playground.
  ///
  /// NB: We need to use an implicit optional as we can only initialise after calling `super.init` in `init` (as we need
  ///     to capture `self`).
  private let haskellSession: HaskellSession!

  /// The text attributes to be applied to all text in the code text views. (Currently, they are fixed.)
  ///
  private let codeTextAttributes: NSDictionary = {
    let menlo13 = NSFont(name: "Menlo-Regular", size:13)!
    return [NSFontAttributeName: menlo13]
  }()

//  /// The text attributes to be applied to all text in the result text views. (Currently, they are fixed.)
//  ///
//  private let resultTextAttributes: NSDictionary = {
//    let menlo13        = NSFont(name: "Menlo-Regular", size:13)!
//    let paragraphStyle = NSParagraphStyle.defaultParagraphStyle().mutableCopy() as NSMutableParagraphStyle
//    paragraphStyle.lineBreakMode = .ByTruncatingTail
//    return [NSFontAttributeName: menlo13, NSParagraphStyleAttributeName: paragraphStyle]
//  }()

  private let fontHeight: CGFloat = {
    let x = NSAttributedString(string: "X", attributes: [NSFontAttributeName: NSFont(name: "Menlo-Regular", size:13)!])
    return x.size.height
  }()

  /// Bin to collect issues for this playground
  ///
  private var issues = IssuesForFile(file: kPlaygroundSource, issues: [:])

  // Objects from the results popover nib.
  //
  @IBOutlet private var popover:           NSPopover?               // referenced to retain
  @IBOutlet private var popoverController: NSViewController!        // referenced to retain
  @IBOutlet private var popoverTextView:   NSTextView!              // This is where the textual result goes.

  @IBOutlet private var resultPopover:           NSPopover?         // referenced to retain
  @IBOutlet private var resultPopoverController: NSViewController!  // referenced to retain
  @IBOutlet private var resultPopoverView:       NSView!            // This is where the graphical result goes.

  //MARK: -
  //MARK: Initialisation and deinitialisation

  init?(
    nibName:                    String!,
    bundle:                     NSBundle!,
    projectViewModelPlayground: ProjectViewModelPlayground!,
    diagnosticsHandler:         Issue -> Void)
  {
    self.projectViewModelPlayground = projectViewModelPlayground
    
      // Call the designated initialiser.
    super.init(nibName: nibName, bundle: bundle)

      // Launch a GHC session for this playground.
    haskellSession = HaskellSession(diagnosticsHandler: processIssue(diagnosticsHandler))
//    haskellSKInit()
  }

  required init?(coder: NSCoder) {
    // just to keep the compiler happy...
    self.projectViewModelPlayground = ProjectViewModelPlayground(identifier: "", model: HFMProjectViewModel())!
    haskellSession = HaskellSession(diagnosticsHandler: {severity, filename, line, column, lines, endColumn, message in })
    super.init(coder: coder)
  }

  deinit {
    codeScrollView.stopSynchronising()
  }

  override func awakeFromNib() {

      // Do not re-initialise when a nib is loaded with objects that we are the owner/delegate of — e.g., the popover view.
    if resultStorage != nil { return }

      // Synchronise the scroll views.
    codeScrollView.setSynchronisedScrollView(resultScrollView)
    resultScrollView.setSynchronisedScrollView(codeScrollView)

      // Set up the gutter.
    codeScrollView.hasVerticalRuler = true
    codeScrollView.rulersVisible    = true

      // The size of the playground code view is fixed. We want it to be rigid.
    codeTextView.horizontallyResizable = true

      // For now, we have got a fixed font.
    codeTextView.font = codeTextAttributes[NSFontAttributeName] as? NSFont

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

      // Apply the default style.
    codeTextView.typingAttributes = codeTextAttributes

      // Set up the delegate for the text storage.
    if let textStorage = codeTextView.layoutManager?.textStorage {
      codeStorageDelegate  = CodeStorageDelegate(textStorage: textStorage)
      textStorage.delegate = codeStorageDelegate
    }

      // Set up the delegate and data source for the result view.
    let reloadDataForRow: Int -> () = { [unowned self] (row: Int) in
      let rowSet    = NSIndexSet(index: row)
      let columnSet = NSIndexSet(indexesInRange: NSRange(location: 0, length: 2))
      self.resultTableView.reloadDataForRowIndexes(rowSet, columnIndexes: columnSet)
    }
    resultTableView.setDelegate(self)
    resultStorage = PlaygroundResultStorage(resultTableView.reloadData, reloadDataForRow)
    resultTableView.setDataSource(resultStorage)

      // Get the initial code view contents and enable highlighting.
    codeTextView.string = projectViewModelPlayground.string
    codeTextView.enableHighlighting(tokeniseHaskell(kPlaygroundSource))
    if let backgroundColour = codeTextView.backgroundColor.shadowWithLevel(0.05) {
      resultTableView.backgroundColor = backgroundColour
    }
  }


  //MARK: -
  //MARK: Context module management

  /// Load a new version of the context module.
  ///
  func loadContextModuleIntoPlayground(moduleText: String!, file: String, importPaths: [String]) -> Bool {

      // Load the module text into GHC.
    return haskellSession.loadModuleFromString(moduleText, file: file, importPaths: importPaths)
  }


  //MARK: -
  //MARK: Processing diagnostics

  /// Load a new version of the context module.
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

  /// Execute all commands in the playground from top to bottom.
  ///
  /// A command starts in the first column of the playground and extends over all immediately following lines whose
  /// first character is a whitespace (emulating Haskell's off-side rule).
  ///
  func execute() {
    let gutter = codeScrollView.verticalRulerView as TextGutterView

      // Invalidate old issues.
    gutter.updateIssues(.IssuesPending)

      // Mark all current results as being stale.
    resultStorage.invalidate()

      // To give AppKit an opportunity to update the gutter and results table (render as stale), we schedule the
      // remainder of this function as a continuation.
    dispatch_async(dispatch_get_main_queue(), {
      self.executeWorker()
    })
  }

  func executeWorker() {
    let layoutManager = codeTextView.layoutManager
    let textContainer = codeTextView.textContainer
    let string        = codeTextView.textStorage!.string
    let gutter        = codeScrollView.verticalRulerView as TextGutterView

      // Discard all old issues.
    issues = IssuesForFile(file: issues.file, issues: [:])

    // Extracts one command, while advancing the current character index.
    //
    func extractCommandAtCharIndex(var charIndex: String.Index) -> (String.Index, String, CGFloat) {
      let initialCharIndex = charIndex
      let lineRange        = string.lineRangeForRange(charIndex...charIndex)
      var command          = string[lineRange]
      charIndex            = lineRange.endIndex

        // Collect lines until you find one that has no white space in the first column.
        // FIXME: we need to use a proper Unicode whitespace test
      while string.endIndex > charIndex && (string[charIndex] == " " || string[charIndex] == "\t" || string[charIndex] == "\n") {
        let lineRange = string.lineRangeForRange(charIndex...charIndex)
        command      += string[lineRange]
        charIndex     = lineRange.endIndex
      }
      let span         = string.startIndex..<initialCharIndex
      let firstIndex   = string[span].utf16Count
      let indexLength  = string[initialCharIndex..<charIndex].utf16Count
      let glyphRange = layoutManager!.glyphRangeForCharacterRange(NSRange(location: firstIndex, length: indexLength),
                                                                  actualCharacterRange: nil)
      let rect       = layoutManager!.boundingRectForGlyphRange(glyphRange, inTextContainer:textContainer!)
      return (charIndex, command, rect.size.height)
    }

      // Traverse all commands.
    var firstIndexOfNextCommand: String.Index = string.startIndex
    var commandIndex                          = 0
    while string.endIndex > firstIndexOfNextCommand {

      let lineNumber                   = string.lineNumberAtLocation(firstIndexOfNextCommand)
      let (nextIndex, command, height) = extractCommandAtCharIndex(firstIndexOfNextCommand)
      firstIndexOfNextCommand          = nextIndex

      let (evalResult: AnyObject, evalTypes) = haskellSession.evalExprFromString(command,
                                                                                 source: kPlaygroundSource,
                                                                                 line: lineNumber)
      if let resultScene = spriteKitView(evalResult) {

          // Graphical result with custom presentation view.
        resultStorage.reportResult("«click to display scene»",
                                   scene: resultScene,
                                   type: ", ".join(evalTypes),
                                   height: height,
                                   atCommandIndex: commandIndex)

      } else if let resultText = evalResult as? String {

          // The result is just a string.
        resultStorage.reportResult(resultText,
                                   scene: nil,
                                   type: ", ".join(evalTypes),
                                   height: height,
                                   atCommandIndex: commandIndex)

      } else {

          // No idea what this result is.
        resultStorage.reportResult("«unknown type of result»",
                                   scene: nil,
                                   type: ", ".join(evalTypes),
                                   height: height,
                                   atCommandIndex: commandIndex)
      }
      commandIndex++
    }
    resultStorage.pruneAt(commandIndex)
    resultTableView.reloadData()

      // Display any diagnostics in the gutter.
    if issues.issues.isEmpty {
      gutter.updateIssues(.NoIssues)
    } else {
      gutter.updateIssues(.Issues(issues))
    }
  }
}


// MARK: -
// MARK: Syntax highlighting support

extension PlaygroundController {

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
  }
}


// MARK: -
// MARK: NSTextViewDelegate protocol methods (for the code view)

extension PlaygroundController: NSTextViewDelegate {
  //FIXME: This is provisionally the delegate for the REPL view while it is so simple.

  func textView(textView: NSTextView, doCommandBySelector selector: Selector) -> Bool {
    if textView != codeTextView {
      NSLog("%s: textView:doCommandBySelector from unexpected text view", __FUNCTION__)
      return false
    }

    if (selector == "insertNewline:") {

      self.execute()
      return false

    }
    return false
  }

}

// MARK: -
// MARK: NSTableViewDelegate protocol methods

extension PlaygroundController: NSTableViewDelegate {

  func tableView(tableView: NSTableView, viewForTableColumn column: NSTableColumn, row: Int) -> NSTableCellView? {

    if let result = resultStorage.queryResult(row) {

      let identifier = column.identifier
      switch identifier {
      case kTypeCell:
        if let cell = tableView.makeViewWithIdentifier(identifier, owner: self) as? NSTableCellView {
          cell.textField?.stringValue = result.type
          cell.textField?.textColor   = result.stale ? NSColor.disabledControlTextColor() : NSColor.controlTextColor()
          return cell
        } else { return nil }
      case kValueCell:
        if let cell = tableView.makeViewWithIdentifier(identifier, owner: self) as? NSTableCellView {
          cell.textField?.stringValue = result.value
          cell.textField?.textColor   = result.stale ? NSColor.disabledControlTextColor() : NSColor.controlTextColor()
          return cell
        } else { return nil }
      default:
        return nil
      }

    } else { return nil }
  }

  func tableView(tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
    if let result = resultStorage.queryResult(row) {
      return result.height
    } else { return 0 }
  }

  func tableViewSelectionDidChange(notification: NSNotification) {
    let tableView = notification.object as NSTableView
    let row       = tableView.selectedRow

    if row != -1 {   // If a row is selected...
      if let result = resultStorage.queryResult(row) {

          // Get the frame of the table view cell whose contents is to be displayed in the popover.
        let rowView  = resultTableView.rowViewAtRow(row, makeIfNecessary: false) as? NSView
        if let frame = rowView?.frame {

          if let scene = result.scene {       // result with a SpriteKit scene

            let bundle = NSBundle.mainBundle()
            if !bundle.loadNibNamed("ResultViewPopover", owner: self, topLevelObjects: nil) {
              NSLog("%@: could not load result popover NIB", __FUNCTION__)
            } else {

              // FIXME: move into a file in Utilities
              func clampExtent(extent: CGFloat, min: CGFloat, max: CGFloat) -> CGFloat {
                if extent < min { return min }
                else if extent > max { return max }
                else { return extent }
              }

                // We want to show the entire scene and center it.
              var sceneFrame = scene.calculateAccumulatedFrame()
              if !isfinite(sceneFrame.origin.x) { sceneFrame.origin.x = 0 }
              if !isfinite(sceneFrame.origin.y) { sceneFrame.origin.y = 0 }
              if !isfinite(sceneFrame.size.width)  || sceneFrame.size.width  < 10 { sceneFrame.size.width  = 10 }
              if !isfinite(sceneFrame.size.height) || sceneFrame.size.height < 10 { sceneFrame.size.height = 10 }

              scene.anchorPoint = CGPoint(x: -sceneFrame.origin.x / sceneFrame.size.width,
                                          y: -sceneFrame.origin.y / sceneFrame.size.height)

                // Constrain the popover size.
              let resultSize = CGSize(width:  clampExtent(sceneFrame.size.width,  50, 600),
                                      height: clampExtent(sceneFrame.size.height, 50, 400))
              var resultView = SKView(frame: CGRect(origin: CGPoint(x: (resultSize.width  - sceneFrame.size.width)  / 2,
                                                                    y: (resultSize.height - sceneFrame.size.height) / 2),
                                                    size: sceneFrame.size))
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
              resultPopover?.contentSize = resultSize
              resultPopover?.behavior    = .Semitransient
              resultPopover?.showRelativeToRect(NSRect(origin: frame.origin, size: CGSize(width: 10, height: 15)),
                                                ofView: resultTableView,
                                                preferredEdge: NSMaxYEdge)
            }

          } else {                            // text only result

            let bundle = NSBundle.mainBundle()
            if !bundle.loadNibNamed("ResultPopover", owner: self, topLevelObjects: nil) {
              NSLog("%@: could not load popover NIB", __FUNCTION__)
            } else {

              let resultString = NSAttributedString(string: result.value)

              popoverTextView.textStorage!.setAttributedString(resultString)
              popover?.behavior = .Semitransient

              let textSize         = resultString.size
              popover?.contentSize = textSize.width > 400 ? NSSize(width: 400, height: 300)
                                                          : NSSize(width: textSize.width < 40 ? 40 : textSize.width,
                                                                   height: textSize.height > 300 ? 300 : textSize.height + 15)
              popover?.showRelativeToRect(NSRect(origin: frame.origin, size: CGSize(width: 10, height: 15)),
                                          ofView: resultTableView,
                                          preferredEdge: NSMaxYEdge)
            }
          }
        }
      }
      resultTableView.deselectRow(row)    // .. so that selecting again will call this function again
    }
  }

  //  func tableViewColumnDidMove(_notification: NSNotification) {
  //  }
  //
  //  func tableViewColumnDidResize(_notification: NSNotification) {
  //  }
  //
  //  func tableViewSelectionIsChanging(_notification: NSNotification) {
  //  }
}
