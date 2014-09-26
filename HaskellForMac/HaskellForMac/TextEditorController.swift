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
  private var lineMap: LineTokenMap = LineTokenMap(string: "")

  /// The text attributes to be applied to all text in the code and result text views. (Currently, they are fixed.)
  //
  // FIXME: Unify with 'Playground.swift'
  private let textAttributes: NSDictionary = { () in
    let menlo13 = NSFont(name: "Menlo-Regular", size:13)
    return [NSFontAttributeName: menlo13]
  }()

  // Text file URL.
  //
  //FIXME: this is awful!!
  private var fileURLDuringInit: NSURL?    // only used during set up


  //MARK: -
  //MARK: Initialisation and deinitialisation

  // Initialise the view controller by loading its NIB file and also set the associated file URL.
  //
  init(nibName: String!, bundle: NSBundle!, projectViewModelItem: HFMProjectViewModelItem!, fileURL: NSURL!) {
    viewModelItem     = projectViewModelItem
    fileURLDuringInit = fileURL

    super.init(nibName: nibName, bundle: bundle)

      // We use our gutter class as a ruler for the text view.
    NSScrollView.setRulerViewClass(TextGutterView)
  }

  required init(coder: NSCoder!) {
    NSLog("%s: WARNING: allocating empty project view model item", __FUNCTION__)
    viewModelItem = HFMProjectViewModelItem()
    super.init(coder: coder)
  }

  override func awakeFromNib() {

      // Initialise the path control.
    pathControl.URL = fileURLDuringInit!
    fileURLDuringInit = nil

      // Fixed for now.
    textView.font = textAttributes[NSFontAttributeName] as NSFont;

      // Set up for code editing (not prose).
    textView.automaticDashSubstitutionEnabled   = false
    textView.automaticDataDetectionEnabled      = false
    textView.automaticLinkDetectionEnabled      = false
    textView.automaticQuoteSubstitutionEnabled  = false
    textView.automaticSpellingCorrectionEnabled = false
    textView.automaticTextReplacementEnabled    = false

      // Apply the default style.
    textView.typingAttributes = textAttributes

      // Set up the gutter.
    scrollView.hasVerticalRuler = true
    scrollView.rulersVisible    = true


      // Register ourselves as the delegate for the text storage.
    textView.layoutManager.textStorage.delegate = self

      // Initialise the line map
    if let tokeniser = highlightingTokeniser {
      lineMap = lineTokenMap(textView.string, tokeniser)
    } else {
      lineMap = lineTokenMap(textView.string, { _string in [] })
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

    // FIXME: This should go into StringExtensions via a call through an extension of NSTextStorage to pick up the
    //        editedRange and changeInLength.
    let editedRange    = textView.textStorage.editedRange
    let changeInLength = textView.textStorage.changeInLength
    let oldRange       = editedRange.location ..< (NSMaxRange(editedRange) - changeInLength)
    let string         = textView.textStorage.string
    let editedString   = (string as NSString).substringWithRange(editedRange)
    NSLog("edited range = (pos: %i, len: %i); change in length = %i",
          editedRange.location, editedRange.length, changeInLength)

      // If the line count changed, we need to recompute the line map and update the gutter.
    let lines          = lineMap.lineRange(oldRange)
    let newlineChars   = NSCharacterSet.newlineCharacterSet()
    let didEditNewline = (editedString as NSString).rangeOfCharacterFromSet(newlineChars).location != NSNotFound
                         || lines.endIndex - lines.startIndex > 1
    if didEditNewline {
      if let tokeniser = highlightingTokeniser {
        lineMap = lineTokenMap(textView.string, tokeniser)
      } else {
        lineMap = lineTokenMap(textView.string, { _string in [] })
      }
      scrollView.verticalRulerView.needsDisplay = true
    }
  }
}
