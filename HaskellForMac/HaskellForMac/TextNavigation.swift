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


  // MARK: -
  // Mark: menu actions

  func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem!) -> Bool {
    return true
  }

  func jumpToNextIssue(sender: AnyObject!) {
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
  }

}
