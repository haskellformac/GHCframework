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

      // Trigger highlighting
    if let tokeniser = highlightingTokeniser {
      textView.highlight(tokeniser)
    }
   }
}


// MARK: -
// MARK: Notifications

extension TextEditorController {

  /// Notify the gutter of a new set of issues for the associated file. (This invalidated all previous issues.)
  ///
  func updateIssues(notification: IssueNotification) {
    (scrollView.verticalRulerView as TextGutterView).updateIssues(notification)
  }
}


// MARK: -
// MARK: NSTextStorageDelegate protocol

extension TextEditorController: NSTextStorageDelegate {

  func textStorageDidProcessEditing(notification: NSNotification) {
//    if let tokeniser = highlightingTokeniser {
//      textView.highlight(tokeniser)
//    }
// ^^^causes an exception (as it claims to do glyph generation before processEditing: is finished)          12
  }

}

