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
  ///
  @IBOutlet private weak var pathControl: NSPathControl!
  @IBOutlet private weak var scrollView:  NSScrollView!
  @IBOutlet private      var textView:    CodeView!

  /// Project view model item representing the edited file.
  ///
  dynamic let viewModelItem: HFMProjectViewModelItem

  /// We need to keep the code storage delegate alive as the delegate reference from `NSTextStorage` is unowned.
  ///
  private var codeStorageDelegate: CodeStorageDelegate!

  /// The text attributes to be applied to all text in the code and result text views. (Currently, they are fixed.)
  ///
  // FIXME: Unify with 'Playground.swift'
  private let textAttributes: NSDictionary = { () in
    let menlo13 = NSFont(name: "Menlo-Regular", size:13)!
    return [NSFontAttributeName: menlo13]
  }()

  // Execute once `awakeFromNib` is being called.
  //
  private var awakeAction: () -> Void = {()}


  //MARK: -
  //MARK: Initialisation and deinitialisation

  /// Initialise the view controller by loading its NIB file and also set the associated file URL.
  ///
  init?(nibName: String!, bundle: NSBundle!, projectViewModelItem: HFMProjectViewModelItem!, filePath: String) {
    viewModelItem = projectViewModelItem

    super.init(nibName: nibName, bundle: bundle)

    awakeAction   = { [unowned self] in
      // Initialise the path control (whose referenced isn't initialised yet).
      self.pathControl.URL = NSURL(string: filePath)
    }

      // We use our gutter class as a ruler for the text view.
    NSScrollView.setRulerViewClass(TextGutterView)
  }

  required init?(coder: NSCoder) {
    NSLog("%s: WARNING: allocating empty project view model item", __FUNCTION__)
    viewModelItem = HFMProjectViewModelItem()
    super.init(coder: coder)
  }

  override func awakeFromNib() {

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

      // Set up the delegate for the text storage.
    if let textStorage = textView.layoutManager?.textStorage {
      codeStorageDelegate  = CodeStorageDelegate(textStorage: textStorage)
      textStorage.delegate = codeStorageDelegate
    }

      // Get the intial edited code.
    textView.string = viewModelItem.string;

      // Execute the awake action. (We do that last to ensure all connections are already set up.)
    awakeAction()
    awakeAction = {()}
  }
}

// MARK: -
// MARK: Syntax highlighting

extension TextEditorController {

  func enableHighlighting(tokeniser: HighlightingTokeniser) {

      // If the text view isn't initialised yet, defer the set up until we awake from NIB loading.
    if let textView = self.textView {
      textView.enableHighlighting(tokeniser)
    } else {
      let oldAwakeAction = awakeAction
      awakeAction = { [unowned self] in
        oldAwakeAction()
        self.textView.enableHighlighting(tokeniser)
      }
    }
  }
}


// MARK: -
// MARK: NSTextDelegate protocol methods

extension TextEditorController: NSTextDelegate {

  func textDidChange(notification: NSNotification) {
    viewModelItem.string = textView.string ?? ""
  }
}


// MARK: -
// MARK: Notifications

extension TextEditorController {

  /// Notify the gutter of a new set of issues for the associated file. (This invalidated all previous issues.)
  ///
  func updateIssues(notification: IssueNotification) {
    if let gutter = scrollView.verticalRulerView as? TextGutterView {
      gutter.updateIssues(notification)
      textView.highlight()
    }
  }
}


// MARK: -
// MARK: Forwarded menu actions

extension TextEditorController {

  func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem) -> Bool {
    return textView.validateUserInterfaceItem(sender)
  }

  func jumpToNextIssue(sender: AnyObject!) {
    return textView.jumpToNextIssue(sender)
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    return textView.jumpToPreviousIssue(sender)
  }
}


// MARK: -
// MARK: First responder

extension TextEditorController {

  func makeCodeViewFirstResponder() {
    textView.window?.makeFirstResponder(textView)
  }
}
