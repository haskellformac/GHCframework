//
//  StyledBox.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 19/03/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Customisable colour for `NSBox` seperator, inspired by Daniel Jalkut's `RSColoredBox`.

import Cocoa


@IBDesignable
class StyledBox: NSBox {

  // MARK: -
  // MARK: Customise the line colour when the box is used as a separator.

  @IBInspectable
  var customLineColor: NSColor = NSColor.gridColor()
  
  override func drawRect(dirtyRect: NSRect) {

    if boxType != NSBoxType.Separator { super.drawRect(dirtyRect) }    // only alter the separator style appearance
    else {

      let line = bounds.size.height > bounds.size.width       // is a vertical separator?
                 ? NSRect(x: NSMidX(bounds), y: NSMinY(bounds), width: 1,                 height: bounds.size.height)
                 : NSRect(x: NSMinX(bounds), y: NSMidY(bounds), width: bounds.size.width, height: 1)
      customLineColor.setFill()
      NSRectFill(line)
    }
  }
}
