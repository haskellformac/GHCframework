//
//  StyledSplitView.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 3/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

@IBDesignable
class StyledSplitView: NSSplitView {

  @IBInspectable
  var customDividerColor: NSColor = NSColor.redColor()

  override var dividerColor: NSColor {
    get {
      return customDividerColor
    }
  }
}
