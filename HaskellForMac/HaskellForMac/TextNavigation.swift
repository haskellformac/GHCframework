//
//  TextNavigation.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  HfM-specific navigation functionality for `NSTextView`s used for code. They are identified by using a 
//  `CodeStorageDelegate` as the delegate of their text storage.

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

  override init() {
    indentWidth = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceIndentationWidth)
    super.init()

    NSNotificationCenter.defaultCenter().addObserver(self,
                                                     selector: "userDefaultsDidChange:",
                                                     name: NSUserDefaultsDidChangeNotification,
                                                     object: NSUserDefaults.standardUserDefaults())
  }

  required init?(coder: NSCoder) {
    indentWidth = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceIndentationWidth)
    super.init(coder: coder)

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
// Mark: menu actions

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
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToNextIssue()
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToPreviousIssue()
  }

}
