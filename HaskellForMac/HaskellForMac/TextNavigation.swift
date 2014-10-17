//
//  TextNavigation.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  HfM-specific navigation functionality for `NSTextView`s.

import Cocoa


extension NSTextView {

  /// The line map associated through the delegate of the associated text storage.
  ///
  var lineMap: LineTokenMap? {
    get {
      return (textStorage?.delegate as? CodeStorageDelegate)?.lineMap
    }
  }

  // MARK: -
  // Mark: menu actions

  func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem!) -> Bool {
    return true
  }

  func jumpToNextIssue(sender: AnyObject!) {
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToNextIssue(sender)
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToPreviousIssue(sender)
  }

}
