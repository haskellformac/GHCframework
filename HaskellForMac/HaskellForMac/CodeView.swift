//
//  CodeView.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  HfM-specific navigation functionality for `NSTextView`s used for code. In additon to be `CodeView`s, they are using
//  a `CodeStorageDelegate` as the delegate of their text storage.

import Cocoa


class CodeView: NSTextView {

  /// The line map associated through the delegate of the associated text storage if available.
  ///
  var lineMap: LineTokenMap? {
    get {
      return (textStorage?.delegate as? CodeStorageDelegate)?.lineMap
    }
  }

  /// The gutter view associated through the enclosing scroll view if available.
  ///
  var textGutterView: TextGutterView? {
    get {
      return enclosingScrollView?.verticalRulerView as? TextGutterView
    }
  }

  /// Number of spaces to use for block indentation — determined by application defaults.
  ///
  var indentWidth: Int

  /// We need to switch of responsive scrolling as it leads to the gutter view lagging during scrolling. (The scroll
  /// view's `visibleRect` property lags during responsive scrolling.) Instead, we use layer backing. It also improves
  /// scrolling at updates the view automatically with contents changes, just like responsive scrolling.
  ///
  override static func isCompatibleWithResponsiveScrolling() -> Bool { return false }

  override init(frame frameRect: NSRect, textContainer container: NSTextContainer?)
  {
    indentWidth = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceIndentationWidth)
    super.init(frame: frameRect, textContainer: container)

    wantsLayer = true
    NSNotificationCenter.defaultCenter().addObserver(self,
      selector: "userDefaultsDidChange:",
      name: NSUserDefaultsDidChangeNotification,
      object: NSUserDefaults.standardUserDefaults())
  }

  required init?(coder: NSCoder) {
    indentWidth = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceIndentationWidth)
    super.init(coder: coder)

    wantsLayer = true
    NSNotificationCenter.defaultCenter().addObserver(self,
                                                     selector: "userDefaultsDidChange:",
                                                     name: NSUserDefaultsDidChangeNotification,
                                                     object: NSUserDefaults.standardUserDefaults())
  }

  deinit {
    NSNotificationCenter.defaultCenter().removeObserver(self)
  }

  func userDefaultsDidChange(notification: NSNotification) {
    indentWidth = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceIndentationWidth)
  }
}


// MARK: -
// MARK: Command processing

extension CodeView: NSTextInputClient {

  // We pick out the interesting commands and forward everything else to the `NSTextView` superclass.
  override func doCommandBySelector(selector: Selector) {

    if (selector == "insertTab:") {

      let here = selectedRange().location
      if let lineNumber = string?.lineNumber(lineMap, atLocation: here) {
        let lineStart = string!.indentOf(lineMap!, line: lineNumber)
        insertSpaces(indentWidth - ((here - 1) - lineStart) % indentWidth)    // 'here - 1' as column 1 is leftmost
      }

    } else if (selector == "insertNewline:") {

      let location = selectedRange().location     // location on key press
      super.doCommandBySelector(selector)         // process the newline
      if let currentLine = string?.lineNumber(lineMap, atLocation: location) {
        let indent = string!.indentOf(lineMap!, line: currentLine)
        insertSpaces(indent)                      // indent according to the line we came from
      }

    } else { super.doCommandBySelector(selector) }
  }

  /// Insert the given number of spaces.
  ///
  func insertSpaces(count: Int) {
    insertText(String(count: count, repeatedValue: Character(" ")))
  }
}

// MARK: -
// MARK: menu actions

extension CodeView: NSUserInterfaceValidations {

  override func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem) -> Bool {

      // Actions defined in this extension only apply code views.
    switch sender.action() {
    case "jumpToNextIssue:", "jumpToPreviousIssue:":
        return textGutterView?.issuesAvailable() ?? false
    default: return super.validateUserInterfaceItem(sender)
    }
  }

  func jumpToNextIssue(sender: AnyObject!) {
    textGutterView?.jumpToNextIssue()
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    textGutterView?.jumpToPreviousIssue()
  }

}
