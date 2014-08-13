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


class PlaygroundController: NSViewController {

  // Views in 'Playground.xib'
  //
  @IBOutlet weak var splitView:        NSSplitView!
  @IBOutlet weak var codeScrollView:   SynchroScrollView!
  @IBOutlet weak var resultScrollView: SynchroScrollView!
  @IBOutlet      var codeTextView:     NSTextView!
  @IBOutlet      var resultTextView:   NSTextView!

  /// The GHC session associated with this playground.
  //
  let haskellSession: HaskellSession

  /// The text attributes to be applied to all text in the code and result text views. (Currently, they are fixed.)
  //
  private let textAttributes: NSDictionary = {
    let menlo13        = NSFont(name: "Menlo-Regular", size:13)
    let paragraphStyle = NSParagraphStyle.defaultParagraphStyle().mutableCopy() as NSMutableParagraphStyle
    paragraphStyle.lineBreakMode = .ByTruncatingTail
    return [NSFontAttributeName: menlo13, NSParagraphStyleAttributeName: paragraphStyle]
  }()

  private var startOfCommand: Int = 0   // FIXME: provisional starting location of type command in REPL

  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(
    nibName:              String!,
    bundle:               NSBundle!,
    projectViewModelItem: HFMProjectViewModelItem!,
    diagnosticsHandler:   Issue -> Void)
  {
      // Launch a GHC session for this playground.
    haskellSession = HaskellSession{severity, filename, line, column, lines, endColumn, message in
                       diagnosticsHandler(Issue(
                                            severity: severity,
                                            filename: filename,
                                            line: line,
                                            column: column,
                                            lines: lines,
                                            endColumn: endColumn,
                                            message: message))
                     }

      // Call the designated initialiser.
    super.init(nibName: nibName, bundle: bundle)
  }

  required init(coder: NSCoder!) {
    haskellSession = HaskellSession(diagnosticsHandler: {severity, filename, line, column, lines, endColumn, message in })
    super.init(coder: coder)
  }

  deinit {
    codeScrollView.stopSynchronising()
  }

  override func awakeFromNib() {

      // Synchronise the scroll views.
    codeScrollView.setSynchronisedScrollView(resultScrollView)
    resultScrollView.setSynchronisedScrollView(codeScrollView)

      // Set up the gutter.
    codeScrollView.hasVerticalRuler = true
    codeScrollView.rulersVisible    = true

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


  //MARK: -
  //MARK: Context module management

  /// Load a new version of the context module.
  ///
  func loadContextModuleIntoPlayground(moduleText: String!) -> Bool {

      // Load the module text into GHC.
    return haskellSession.loadModuleFromString(moduleText)
  }

}


//MARK: -
//MARK: NSTextViewDelegate protocol methods

extension PlaygroundController: NSTextViewDelegate {
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
