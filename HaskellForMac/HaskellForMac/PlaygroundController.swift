//
//  PlaygroundController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/07/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

class PlaygroundController: NSViewController, NSTextViewDelegate {

  // Views in 'Playground.xib'
  //
  @IBOutlet weak var splitView: NSSplitView!
  @IBOutlet weak var codeScrollView: SynchroScrollView!
  @IBOutlet weak var resultScrollView: NSScrollView!
  @IBOutlet      var codeTextView: NSTextView!
  @IBOutlet      var resultTextView: NSTextView!

  /// The GHC session associated with this playground.
  //
  let haskellSession: HaskellSession

  /// The text attributes to be applied to all text in the code and result text views. (Currently, they are fixed.)
  //
  private let textAttributes: NSDictionary

  private var startOfCommand: Int = 0   // FIXME: provisional starting location of type command in REPL


  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(nibName: String, bundle: NSBundle, item: HFMProjectViewModelItem) {

      // Get GHC going.
    haskellSession = HaskellSession()

      // Determine the default text attributes.
    let menlo13        = NSFont(name: "Menlo-Regular", size:13)
    let paragraphStyle = NSParagraphStyle.defaultParagraphStyle().mutableCopy() as NSMutableParagraphStyle
    paragraphStyle.lineBreakMode = .ByTruncatingTail
    textAttributes = [NSFontAttributeName: menlo13, NSParagraphStyleAttributeName: paragraphStyle]

      // Call the designated initialiser.
    super.init(nibName: nibName, bundle: bundle)

      // Register with the model view item that represents the Haskell file providing the context for this playground.
    item.loadString = loadContextModuleIntoPlayground
  }

  deinit {
    codeScrollView.stopSynchronising()
  }

  override func awakeFromNib() {

      // Synchronise the scroll views.
    codeScrollView.setSynchronisedScrollView(resultScrollView)

      // The size of the playground text views is fixed. We want them to be rigid.
    codeTextView.horizontallyResizable   = true
    resultTextView.horizontallyResizable = true

      // For now, we have got a fixed font.
    codeTextView.font   = textAttributes[NSFontAttributeName] as NSFont
    resultTextView.font = textAttributes[NSFontAttributeName] as NSFont

      // Apply the default style.
    codeTextView.defaultParagraphStyle   = textAttributes[NSParagraphStyleAttributeName] as NSParagraphStyle
    codeTextView.typingAttributes        = textAttributes
    resultTextView.defaultParagraphStyle = textAttributes[NSParagraphStyleAttributeName] as NSParagraphStyle
    resultTextView.typingAttributes      = textAttributes
  }

  // Module loading
  //
  private func loadContextModuleIntoPlayground(moduleText: String!) {

      // Load the module text into GHC.
    let loadResult = haskellSession.loadModuleFromString(moduleText) + "\n\n"

      // Print any errors to the result view.
    let attrText = NSAttributedString(string: loadResult, attributes:textAttributes)
    resultTextView.textStorage.setAttributedString(attrText)
    resultTextView.scrollRangeToVisible(NSRange(location: resultTextView.textStorage.length, length: 0))
  }


  //MARK: -
  //MARK: NSTextViewDelegate protocol methods

  //FIXME: This is provisionally the delegate for the REPL view while it is so simple.

  func textView(textView: NSTextView, doCommandBySelector selector: Selector) -> Bool {
    if textView != codeTextView {
      NSLog("%s: textView:doCommandBySelector from unexpected text view", __FUNCTION__)
      return false
    }

    if (selector == "insertNewline:") {

      let userCommand = (textView.textStorage.string as NSString).substringFromIndex(startOfCommand)

        // Move insertion point to the end
      codeTextView.setSelectedRange(NSRange(location: codeTextView.textStorage.length, length: 0))
      codeTextView.insertNewline(self)

        // Execute command
      let evalResult = haskellSession.evalExprFromString(userCommand)

        // Insert result in the REPL area
      let attrResult = NSAttributedString(string: evalResult + "\n", attributes:textAttributes)
      resultTextView.textStorage.appendAttributedString(attrResult)

        // Remember the position where the output ended.
      startOfCommand = codeTextView.selectedRange().location + 1  // '+1' to account for the newline
    }
    return false
  }

}
