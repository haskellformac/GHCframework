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

  // All issues currently flagged for the file annotated by this gutter, their sorted sequence of line numbers, and
  // the sequence index of the currently or last displayed issue. (If there is no current issue, the index is invalid.)
  //
  private var issues:                 Issues = [:]
  private var issueSequence:          [Line] = []        // cache of `sorted(issues.keys)`
  private var currentIndexInSequence: Int    = -1

  // Whether the issues set is current or invalidated (i.e., new ones are being computed).
  //
  private var markIssuesAsInvalid: Bool = false
  
  // Text attributes for line numbers (dependent on the current font size determined by theme preferences)
  //
  // Font alternatives: Gill Sans, Century Gothic, Franklin Gothic, Futura (small, semibold) & Lucida Sans Typewriter
  private let lineNumberFontName   = "Calibri"
  private let lineNumberColour     = NSColor(deviceWhite: 0.5, alpha: 1)
  private var lineNumberFont       : NSFont {
    let size = round(CGFloat(ThemesController.sharedThemesController().currentFontSize) * 0.9)
    return NSFont(name: lineNumberFontName, size: size) ?? NSFont(name: "Menlo-Regular", size: 12)!
  }
  private var lineNumberAttributes : [String: AnyObject] {
    get {
      return [ NSFontAttributeName           : lineNumberFont
             , NSForegroundColorAttributeName: lineNumberColour
             ]
    }
  }

  // Background colours for issues
  //
  private let errorBgColour           = NSColor(red: 1, green: 0.4, blue: 0,   alpha: 0.5)
  private let disabledErrorBgColour   = NSColor(red: 1, green: 0.4, blue: 0,   alpha: 0.2)
  private let warningBgColour         = NSColor(red: 1, green: 1,   blue: 0.3, alpha: 0.5)
  private let disabledWarningBgColour = NSColor(red: 1, green: 1,   blue: 0.3, alpha: 0.2)

  private let errorSymbol   = "⛔️"
  private let warningSymbol = "❕"

  // Gap between the line numbers and the bounding box of the ruler.
  //
  private let margin: CGFloat = 3

  /// The text attributes for diagnostics in popups (dependent on the current font size determined by theme preferences)
  //
  private let diagnosticsFontName       = "Menlo-Regular"
  private var diagnosticsTextAttributes : [String: AnyObject] {
    get {
      let size = round(CGFloat(ThemesController.sharedThemesController().currentFontSize) * 0.9)
      let font = NSFont(name: diagnosticsFontName, size: size) ?? NSFont(name: "Menlo-Regular", size: 12)!
      return [NSFontAttributeName: font]
    }
  }

  // Objects from the diagnostics popover nib.
  //
  @IBOutlet private      var popover:           NSPopover?           // referenced to retain
  @IBOutlet private      var popoverController: NSViewController!    // referenced to retain
  @IBOutlet private weak var popoverTextField:  NSTextField!         // This is where the diagnostics goes.


  // MARK: -
  // MARK: Initialisation

  override init(scrollView: NSScrollView!, orientation: NSRulerOrientation) {
    if orientation == .HorizontalRuler {
      NSLog("%s: 'TextGutterView' does not support a horizontal orientation", __FUNCTION__)
    }

    super.init(scrollView: scrollView, orientation: orientation)

    self.clientView = scrollView.documentView as? NSView
  }

  required init?(coder: NSCoder) {
    super.init(coder: coder)
  }


  // MARK: -
  // MARK: Notifications

  /// Notify the gutter of a new set of issues for the associated file. (This invalidated all previous issues.)
  ///
  func updateIssues(notification: IssueNotification) {
    switch notification {

    case .NoIssues:
      markIssuesAsInvalid    = false
      issues                 = [:]
      issueSequence          = []
      currentIndexInSequence = -1

    case .IssuesPending:
      markIssuesAsInvalid = true

    case .Issues(let issuesForFile):
      markIssuesAsInvalid    = false
      issues                 = issuesForFile.issues
      currentIndexInSequence = -1
      issueSequence          = sorted(issues.keys)
    }
    needsDisplay = true
  }


  // MARK: -
  // MARK: Issue navigation

  /// Determine whether we have any issues to navigate.
  ///
  func issuesAvailable() -> Bool {
    return !issues.isEmpty
  }

  /// Determine the following index into `issueSequence` (if any).
  ///
  private func sequenceIndexOfNextIssue() -> Int? {

      // no issues => no next one
    if issues.isEmpty { return nil }

      // invalid current index or current index is the last one => first issue is the next one
    if currentIndexInSequence < 0
      || currentIndexInSequence >= issueSequence.endIndex
      || advance(currentIndexInSequence, 1) == issueSequence.endIndex { return 0 }

      // otherwise, just advance the current index
    return advance(currentIndexInSequence, 1)
  }

  /// Display the next issue in the issue sequence (if any).
  ///
  func jumpToNextIssue() {
    if let index = sequenceIndexOfNextIssue() {
      jumpToIssueAtIndex(index)
    }
  }

  /// Determine the previous index into `issueSequence` (if any).
  ///
  private func sequenceIndexOfPreviousIssue() -> Int? {

      // no issues => no next one
    if issues.isEmpty { return nil }

      // invalid current index or current index is the first one => last issue is the next one
    if currentIndexInSequence < 0
      || currentIndexInSequence >= issueSequence.endIndex
      || currentIndexInSequence == issueSequence.startIndex { return advance(issueSequence.endIndex, -1) }

    // otherwise, just decrease the current index
    return advance(currentIndexInSequence, -1)
  }

  /// Display the previous issue in the issue sequence (if any).
  ///
  func jumpToPreviousIssue() {
    if let index = sequenceIndexOfPreviousIssue() {
      jumpToIssueAtIndex(index)
    }
  }

  /// Display the issue at the given line; if it is already being displayed, hide it.
  ///
  func jumpToIssueAtLine(lineNumber: Line) {
    // FIXME: This is O(n), we could use binary search instead if it turns out to be a bottleneck.
    var current: Int = 0
    while (current < issueSequence.endIndex && issueSequence[current] <= lineNumber) {
      if issueSequence[current] == lineNumber { jumpToIssueAtIndex(current) }
      current++
    }
  }

  /// Display the issue whose line is determined by the given sequence index; if it is already being displayed, hide it.
  ///
  private func jumpToIssueAtIndex(index: Int) {
    if index < 0 || index >= issueSequence.endIndex { return }    // ensure the index is valid

      // Remove any currently shown the popover and, if the shown popover was for the same line, leave it at that.
    if popover != nil && popover!.shown {
      popover!.close()
      if index == currentIndexInSequence { return }
    }
    currentIndexInSequence = index

    let lineNumber = issueSequence[index]
    let lineMap    = (self.clientView as? CodeView)?.lineMap

    if let charIdx = lineMap?.startOfLine(lineNumber) {

        // If there is an issue at this line, display the error message in a popup view.
      if let issues = issues[lineNumber] {


        var objs: NSArray?
        let (gutterRect, _) = gutterRectForCharRange(NSRange(location: charIdx, length: 0))
        let bundle          = NSBundle.mainBundle()
        if !bundle.loadNibNamed("DiagnosticsPopover", owner: self, topLevelObjects: nil) {
          NSLog("%@: could not load popover NIB", __FUNCTION__)
        } else {

          let msg = NSAttributedString(string: issues.map{$0.message}.reduce(""){$0 + $1},
            attributes: diagnosticsTextAttributes)

            // By default, we go for the width of the window in the NIB and compute the required height given the
            // string we need to display.
          let width = popover?.contentViewController?.view.bounds.size.width ?? 600
          popoverTextField.attributedStringValue   = msg
          popoverTextField.preferredMaxLayoutWidth = width
          popoverTextField.sizeToFit()

          let textSize     = popoverTextField.intrinsicContentSize
          let contentWidth = textSize.width < width ? max(textSize.width, 50) : width
          if textSize.width < contentWidth {
            // FIXME: center the content when it is smaller than the popover, but how?
          }
          let contentSize    = NSSize(width: contentWidth, height: textSize.height > 500 ? 500 : textSize.height)
          // FIXME: How can we determine the constants programatically? NSScrollView.frameSizeForContentSize(_:_:_:_:_:)
          //        doesn't seem to work as exepcted.
          let scrollViewSize = CGSize(width: contentSize.width + 4, height: contentSize.height + 4)

          popover?.contentSize               = scrollViewSize
          popover?.behavior                  = .Semitransient
          popover?.showRelativeToRect(gutterRect, ofView:self, preferredEdge: NSMaxYEdge)
        }
      }
    }
  }


  // MARK: -
  // MARK: Custom drawing

  override func viewWillDraw() {
    ruleThickness = ("9999" as NSString).sizeWithAttributes(lineNumberAttributes).width + margin * 2;
      // setting 'ruleThickness' in init leads to a loop
  }

  override func drawHashMarksAndLabelsInRect(rect: NSRect) {
    let textView      = self.clientView as CodeView
    let layoutManager = textView.layoutManager!
    let textContainer = textView.textContainer
    let string        = textView.textStorage!.string
    let visibleRect   = self.scrollView!.documentVisibleRect

      // Draw the background and the divider line.
    gutterColour(ThemesController.sharedThemesController().currentTheme).setFill()
    NSBezierPath(rect: rect).fill()
    dividerColour(ThemesController.sharedThemesController().currentTheme).setStroke()
    NSBezierPath(rect: NSRect(x: CGRectGetMaxX(rect) - 0.5, y: CGRectGetMinY(rect), width: 1, height: rect.size.height)).stroke()

      // All visible glyphs and all visible characters
    let glyphRange = layoutManager.glyphRangeForBoundingRectWithoutAdditionalLayout(visibleRect,
                                                                                    inTextContainer: textContainer!)
    let charRange  = layoutManager.characterRangeForGlyphRange(glyphRange, actualGlyphRange: nil)

      // Line number of first visible line
    let firstLineNumber = string.lineNumber(textView.lineMap, atLocation: charRange.location)

      // Remove the diagnostics popover if it is visible.
    if popover != nil && popover!.shown {
      popover!.close()
    }

      // Iterate through the line numbers of all visible lines
      //
      // NB: The line number of the first visible line may not be visble (if the line wraps), but clipping handles that
      //     for us.
    var lineNumber = firstLineNumber
    var charIndex  = charRange.location
    while charIndex < NSMaxRange(charRange) {

      let lineRange = (string as NSString).lineRangeForRange(NSRange(location: charIndex, length: 0))

        // Draw the number for the current line
      let (gutterRect, middleline) = gutterRectForCharRange(lineRange)
      drawLineNumber(lineNumber, rect: gutterRect, middleline: middleline)

        // Advance to next line
      lineNumber++
      charIndex = NSMaxRange(lineRange)
    }

      // Make sure to add an extra line number if we have an empty last line.
    if (string as NSString).length == NSMaxRange(charRange)
        && (string as NSString).length > 0
        && string[advance(string.endIndex, -1)] == "\n" {
      let rect = layoutManager.extraLineFragmentRect
      if rect != NSZeroRect {
        drawLineNumber(
          lineNumber,
          rect: NSRect(x: 0, y: rect.origin.y - visibleRect.origin.y, width: ruleThickness, height: rect.size.height),
          middleline: rect.size.height / 2)
      }
    }
  }

  // Computes the rect of the gutter area associated with the given character range (as well as the middle lines of the
  // first text line) — the overall range may span multiple text lines due to line wrapping.
  //
  private func gutterRectForCharRange(charRange: NSRange) -> (NSRect, CGFloat) {
    let textView      = self.clientView as NSTextView
    let layoutManager = textView.layoutManager!
    let textContainer = textView.textContainer
    let string        = textView.textStorage!.string
    let visibleRect   = self.scrollView!.documentVisibleRect

      // Draw the number for the current line
    let firstIndex = layoutManager.glyphIndexForCharacterAtIndex(charRange.location)
    let firstRect  = layoutManager.lineFragmentRectForGlyphAtIndex(firstIndex,
                                                                   effectiveRange: nil)
    let lastIndex  = (charRange.length == 0) ? firstIndex
                                             : layoutManager.glyphIndexForCharacterAtIndex(NSMaxRange(charRange) - 1)
    let lastRect   = layoutManager.lineFragmentRectForGlyphAtIndex(lastIndex,
                                                                   effectiveRange: nil)
    return (NSRect(x: 0,
                   y: firstRect.origin.y - visibleRect.origin.y,
                   width: ruleThickness,
                   height: lastRect.origin.y - firstRect.origin.y + lastRect.size.height),
            firstRect.size.height / 2)
  }

  // Draws the given number in the gutter, such that the glyphs are vertically centred around the given middle line.
  // The middle line is centred with the first row of the line and is relative to the rect, which specifies the extent
  // of the gutter area belonging to this line (for background drawing).
  //
  private func drawLineNumber(lineNumber: UInt, rect: NSRect, middleline: CGFloat) {

      // Draw the background.
    let lineIssuesOpt = issues[lineNumber]
    let maxSeverity   = (lineIssuesOpt == nil) ? nil : maxSeverityOfIssues(lineIssuesOpt!)
    if let severity = maxSeverity {

      switch severity {
      case .Error:   (markIssuesAsInvalid ? disabledErrorBgColour   : errorBgColour  ).setFill()
      case .Warning: (markIssuesAsInvalid ? disabledWarningBgColour : warningBgColour).setFill()
      case .Other:   ()
      }
      NSBezierPath(rect: rect).fill()
    }

      // Draw the number.
    let offset       = (lineNumberFont.ascender - lineNumberFont.capHeight) / 2   // Glyphs are not centered in b-box
    let numberString = lineNumber.description as NSString
    let size         = numberString.sizeWithAttributes(lineNumberAttributes)
    numberString.drawAtPoint(NSPoint(x: NSMaxX(rect) - margin - size.width,
                                     y: rect.origin.y + middleline - size.height / 2 - offset),
                             withAttributes: lineNumberAttributes)

      // Draw an issue symbol if any.
    if let severity = maxSeverity {

      var symbol: String
      switch severity {
      case .Error:   symbol = errorSymbol
      case .Warning: symbol = warningSymbol
      case .Other:   return
      }
      symbol.drawAtPoint(NSPoint(x: rect.origin.x,
                                 y: rect.origin.y + middleline - size.height / 2),
                                 withAttributes: lineNumberAttributes)
    }
  }
}


// MARK: -
// MARK: 'NSView' delegate methods

extension TextGutterView {
  override func mouseDown(event: NSEvent) {
    let textView      = self.clientView as CodeView
    let layoutManager = textView.layoutManager
    let textContainer = textView.textContainer!
    let string        = textView.textStorage!.string

    if let visibleRect = self.scrollView?.documentVisibleRect {

        // Determine the line corresponding to the mouse down event.
      let rulerLoc   = self.convertPoint(event.locationInWindow, fromView: nil)
      let textLoc    = NSPoint(x: 0, y: visibleRect.origin.y + rulerLoc.y)
      let glyphIndex = layoutManager!.glyphIndexForPoint(textLoc, inTextContainer: textContainer)
      let charIndex  = layoutManager!.characterIndexForGlyphAtIndex(glyphIndex)
      let lineNumber = string.lineNumber(textView.lineMap, atLocation: charIndex)

      jumpToIssueAtLine(lineNumber)
    }
  }
}

extension TextGutterView: NSUserInterfaceValidations {

  func validateUserInterfaceItem(sender: NSValidatedUserInterfaceItem) -> Bool {

      // Actions defined in this extension only apply code views.
    switch sender.action() {
    case "jumpToNextIssue:", "jumpToPreviousIssue:":
      return issuesAvailable() ?? false
    default: return false
    }
  }

  func jumpToNextIssue(sender: AnyObject!) {
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToNextIssue()
  }

  func jumpToPreviousIssue(sender: AnyObject!) {
    (enclosingScrollView?.verticalRulerView as? TextGutterView)?.jumpToPreviousIssue()
  }
  
}
