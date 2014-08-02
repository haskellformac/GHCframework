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

  var startOfCommand: Int = 0   // FIXME: provisional starting location of type command in REPL


  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(nibName: String, bundle: NSBundle, item: HFMProjectViewModelItem) {

      // Get GHC going.
    haskellSession = HaskellSession()

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

      // For now, we have got a fixed font.
    codeTextView.font   = NSFont(name: "Menlo-Regular", size:13)
    resultTextView.font = NSFont(name: "Menlo-Regular", size:13)
  }

  // Module loading
  //
  private func loadContextModuleIntoPlayground(moduleText: String!) {

      // Load the module text into GHC.
    let loadResult = haskellSession.loadModuleFromString(moduleText) + "\n\n"

      // Print any errors to the result view.
    let menlo13    = NSFont(name: "Menlo-Regular", size:13)
    let attrText   = NSAttributedString(string: loadResult, attributes:[NSFontAttributeName: menlo13])
    resultTextView.textStorage.setAttributedString(attrText)
    resultTextView.scrollRangeToVisible(NSRange(location: resultTextView.textStorage.length, length: 0))

      // Remember the position where the output ended.
    startOfCommand = resultTextView.selectedRange().location
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
      let menlo13    = NSFont(name: "Menlo-Regular", size:13)
      let attrResult = NSAttributedString(string: evalResult, attributes:[NSFontAttributeName: menlo13])
      resultTextView.textStorage.appendAttributedString(attrResult)

      startOfCommand += 1   // '+1' to account for the newline

    }
    return false
  }

}
