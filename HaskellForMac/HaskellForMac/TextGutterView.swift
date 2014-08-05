//
//  TextGutterView.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 5/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This totally guts the drawing functionality of 'NSRuleView' and replaces it with a vertical ruler of line numbers.
//  The benefit we get out of subclassing 'NSRuleView' is the integration with the enclosing scroll view and the
//  scrolled content.

import Cocoa

class TextGutterView: NSRulerView {

  // Text attributes for line numbers
  //
  private let textAttributes = [NSFontAttributeName: NSFont.systemFontOfSize(NSFont.smallSystemFontSize())]

  // Gap between the line numbers and the bounding box of the ruler.
  //
  private let margin: CGFloat = 4


  // MARK: -
  // MARK: Initialisation

  override init(scrollView: NSScrollView!, orientation: NSRulerOrientation) {
    if orientation == .HorizontalRuler {
      NSLog("%s: 'TextGutterView' does not support a horizontal orientation", __FUNCTION__)
    }

    super.init(scrollView: scrollView, orientation: orientation)
  }

  required init(coder: NSCoder) {
    super.init(coder: coder)
  }


  // MARK: -
  // MARK: Custom drawing

  override func viewWillDraw() {
    ruleThickness = ("9999" as NSString).sizeWithAttributes(textAttributes).width + margin * 2;
      // setting 'ruleThickness' in init leads to a loop
  }

  override func drawHashMarksAndLabelsInRect(rect: NSRect) {
    drawLineNumber(42, middleline: rect.origin.y)
  }

  // Draws the given number in the gutter, such that the glyphs' baseline is as specified. The baseline is given
  // relative to the gutter view.
  //
  private func drawLineNumber(lineNumber: Int, middleline: CGFloat) {
    let numberString = lineNumber.description as NSString
    let size         = numberString.sizeWithAttributes(textAttributes)
    numberString.drawAtPoint(NSPoint(x: ruleThickness - margin - size.width,
                                     y: middleline - size.height / 2),
                             withAttributes: textAttributes)
  }
}
