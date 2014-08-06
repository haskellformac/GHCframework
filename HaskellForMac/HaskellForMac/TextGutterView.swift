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
  private let textAttributes = [NSFontAttributeName:            NSFont.systemFontOfSize(NSFont.smallSystemFontSize()),
                                NSForegroundColorAttributeName: NSColor(deviceWhite: 0.5, alpha: 1)]

  // Gap between the line numbers and the bounding box of the ruler.
  //
  private let margin: CGFloat = 3


  // MARK: -
  // MARK: Initialisation

  override init(scrollView: NSScrollView!, orientation: NSRulerOrientation) {
    if orientation == .HorizontalRuler {
      NSLog("%s: 'TextGutterView' does not support a horizontal orientation", __FUNCTION__)
    }

    super.init(scrollView: scrollView, orientation: orientation)

    self.clientView = scrollView.documentView as NSView
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
    let textView      = self.clientView as NSTextView
    let layoutManager = textView.layoutManager
    let textContainer = textView.textContainer
    let string        = textView.textStorage.string
    let visibleRect   = self.scrollView.documentVisibleRect

      // All visible glyphs and all visible characters
    let glyphRange    = layoutManager.glyphRangeForBoundingRectWithoutAdditionalLayout(visibleRect,
                                                                                       inTextContainer: textContainer)
    let charRange     = layoutManager.characterRangeForGlyphRange(glyphRange, actualGlyphRange: nil)

      // Line number of first visible line
    let firstLineNumber = string.lineNumberAtLocation(charRange.location)

      // Iterate through the line numbers of all visible lines
      //
      // NB: The line number of the first visible line may not be visble (if the line wraps), but clipping handles that
      //     for us.
    var lineNumber = firstLineNumber
    var charIndex  = charRange.location
    while charIndex < NSMaxRange(charRange) {

      let lineRange = (string as NSString).lineRangeForRange(NSRange(location: charIndex, length: 0))

        // Draw the number for the current line
      let offset = layoutManager.lineFragmentRectForGlyphAtIndex(layoutManager.glyphIndexForCharacterAtIndex(lineRange.location),
                                                                 effectiveRange: nil)
      drawLineNumber(lineNumber, middleline: offset.origin.y - visibleRect.origin.y + offset.size.height / 2)

        // Advance to next line
      lineNumber++
      charIndex = NSMaxRange(lineRange)
    }
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

extension NSString {
  func lineNumberAtLocation(loc: Int) -> Int {
    switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
    case 0: return 1
    case let start: return lineNumberAtLocation(start - 1) + 1
    }
  }
}
