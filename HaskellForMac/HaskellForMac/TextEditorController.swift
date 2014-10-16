//
//  TextEditorController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 4/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

class TextEditorController: NSViewController {

  /// Content views of the header editor
  //
  @IBOutlet weak var pathControl: NSPathControl!
  @IBOutlet weak var scrollView:  NSScrollView!
  @IBOutlet      var textView:    NSTextView!

  /// Project view model item representing the edited file.
  //
  dynamic let viewModelItem: HFMProjectViewModelItem

  /// Tokeniser to be used for syntax highlighting if a tokeniser is available for the edited file type.
  ///
  /// This variable is set by the context controller.
  ///
  var highlightingTokeniser: HighlightingTokeniser?

  /// Map from line numbers to pairs of character index (where the line starts) and tokens on that line.
  ///
  /// This value is mutable as it changes while the underlying text storage is being edited.
  ///
  private final var lineMap: LineTokenMap = LineTokenMap(string: "")

  /// The text attributes to be applied to all text in the code and result text views. (Currently, they are fixed.)
  //
  // FIXME: Unify with 'Playground.swift'
  private let textAttributes: NSDictionary = { () in
    let menlo13 = NSFont(name: "Menlo-Regular", size:13)!
    return [NSFontAttributeName: menlo13]
  }()

  // Text file URL.
  //
  //FIXME: this is awful!!
  private var filePathDuringInit: String?    // only used during set up


  //MARK: -
  //MARK: Initialisation and deinitialisation

  // Initialise the view controller by loading its NIB file and also set the associated file URL.
  //
  init?(nibName: String!, bundle: NSBundle!, projectViewModelItem: HFMProjectViewModelItem!, filePath: String) {
    viewModelItem      = projectViewModelItem
    filePathDuringInit = filePath

    super.init(nibName: nibName, bundle: bundle)

      // We use our gutter class as a ruler for the text view.
    NSScrollView.setRulerViewClass(TextGutterView)
  }

  required init?(coder: NSCoder) {
    NSLog("%s: WARNING: allocating empty project view model item", __FUNCTION__)
    viewModelItem = HFMProjectViewModelItem()
    super.init(coder: coder)
  }

  override func awakeFromNib() {

      // Initialise the path control.
    pathControl.URL = NSURL(string: filePathDuringInit!)
    filePathDuringInit = nil

      // Fixed for now.
    textView.font = textAttributes[NSFontAttributeName] as? NSFont;

      // Set up for code editing (not prose).
    textView.automaticDashSubstitutionEnabled   = false
    textView.automaticDataDetectionEnabled      = false
    textView.automaticLinkDetectionEnabled      = false
    textView.automaticQuoteSubstitutionEnabled  = false
    textView.automaticSpellingCorrectionEnabled = false
    textView.automaticTextReplacementEnabled    = false

      // FIXME: How can we do that in a locale-independent way.
    var contextMenu = NSTextView.defaultMenu()
    if let item = contextMenu?.itemWithTitle("Spelling and Grammar") { contextMenu?.removeItem(item) }
    if let item = contextMenu?.itemWithTitle("Substitutions")        { contextMenu?.removeItem(item) }
    if let item = contextMenu?.itemWithTitle("Layout Orientation")   { contextMenu?.removeItem(item) }
    textView.menu = contextMenu

      // Apply the default style.
    textView.typingAttributes = textAttributes

      // Set up the gutter.
    scrollView.hasVerticalRuler = true
    scrollView.rulersVisible    = true


      // Register ourselves as the delegate for the text storage.
    textView.layoutManager?.textStorage?.delegate = self

      // Initialise the line map
    if let tokeniser = highlightingTokeniser {
      lineMap = lineTokenMap(textView.string!, tokeniser)
    } else {
      lineMap = lineTokenMap(textView.string!, { _string in [] })
    }
    textView.highlight(lineMap)
  }
}


// MARK: -
// MARK: Notifications

extension TextEditorController {

  /// Notify the gutter of a new set of issues for the associated file. (This invalidated all previous issues.)
  ///
  func updateIssues(notification: IssueNotification) {
    (scrollView.verticalRulerView as TextGutterView).updateIssues(notification)
    highlight()
  }
}


// MARK: -
// MARK: Syntax highlighting

extension TextEditorController {

  func highlight() {
    textView.highlight(lineMap)
  }
}


// MARK: -
// MARK: NSTextStorageDelegate protocol

extension TextEditorController: NSTextStorageDelegate {

  func textStorageDidProcessEditing(notification: NSNotification) {
    let editedRange    = textView.textStorage!.editedRange
    let changeInLength = textView.textStorage!.changeInLength

      // We need to delay fixing the temporary attributes until after the text storage is done processing the current
      // change.
    dispatch_async(dispatch_get_main_queue(), {
      self.highlightingAfterEditing(editedRange, changeInLength: changeInLength)
    })
  }

  func highlightingAfterEditing(editedRange: NSRange, changeInLength: Int) {

    // FIXME: this should go into SyntaxHighlighting via a call through an extension of NSTextStorage
    //        to pick up the editedRange and changeInLength. (Disentangle from NSTextView to support testing.)
    let oldRange       = editedRange.location ..< (NSMaxRange(editedRange) - changeInLength)
    let string         = textView.textStorage!.string
    let editedString   = (string as NSString).substringWithRange(editedRange)
//    NSLog("edited range = (pos: %i, len: %i); change in length = %i",
//          editedRange.location, editedRange.length, changeInLength)

      // If the line count changed, we need to recompute the line map and update the gutter.
    let lines          = lineMap.lineRange(oldRange)
    let rescanOffsets  = lineRangeRescanOffsets(lineMap, lines)  // NB: need to use old range, because of old lines map
    let newlineChars   = NSCharacterSet.newlineCharacterSet()
    let didEditNewline = (editedString as NSString).rangeOfCharacterFromSet(newlineChars).location != NSNotFound
                         || lines.endIndex - lines.startIndex > 1
      // FIXME: The above predicate is too coarse. Even if `lines.endIndex - lines.startIndex > 1`, that is ok in that
      //        the number of lines didn't *change*, iff the number of newline characters in the edited string is equal
      //        to `lines.endIndex - lines.startIndex`.
    if didEditNewline {

        // line count changed => compute a completely new line map
      if let tokeniser = highlightingTokeniser {
        lineMap = lineTokenMap(textView.string!, tokeniser)
      } else {
        lineMap = lineTokenMap(textView.string!, { _string in [] })
      }
      scrollView.verticalRulerView!.needsDisplay = true    // update the line numbering in the gutter

    } else if let tokeniser = highlightingTokeniser {

        // line count stayed the same => may determine lines for rescan with the old (outdated) map
        // FIXME: this is dodgy and really only works, because we only get here in the case where only one line was 
        //        modified; we would probably need split `rescanTokenLines` into the fixing up of the start indicies
        //        and the recomputation of the token array, and then. compute the lineRange in the middle...
      let rescanLines = clampRange(extendRange(lines, rescanOffsets), 1..<lineMap.lastLine)
      lineMap = rescanTokenLines(lineMap, rescanLines, textView.string!, tokeniser)

    }

      // For highlighting, we interested in the new set of lines.
    let editedLines      = lineMap.lineRange(fromNSRange(editedRange))
    let rehighlightLines = clampRange(extendRange(editedLines, rescanOffsets), 1..<lineMap.lastLine)
    textView.highlight(lineMap, lineRange:rehighlightLines)
  }
}
