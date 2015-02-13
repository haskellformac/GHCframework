//
//  CodeStorageDelegate.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class implements an `NSTextStorageDelegate` for code editing. It keeps track of line information needed for
//  syntax highlighting, line numbering, and navigation.
//
//  As this class stores information derived from one particular text storage, a unique instance is used for each
//  text storage instance.

import Cocoa


final class CodeStorageDelegate: NSObject {

  /// The associated text storage. (Can be strong as the text storage doesn't own the delegate. The lifetime of the two
  /// is the same.)
  ///
  private var textStorage: NSTextStorage

  /// Tokeniser to be used for syntax highlighting if a tokeniser is available for the edited file type.
  ///
  /// This variable is set by the context controller.
  ///
  private var highlightingTokeniser: HighlightingTokeniser?

  /// The currently used theme of the associated code view.
  ///
  var themeDict: ThemeDictionary = [:]

  /// Map from line numbers to pairs of character index (where the line starts) and tokens on that line.
  ///
  /// This value is mutable as it changes while the underlying text storage is being edited.
  ///
  var lineMap: LineTokenMap

  init(textStorage: NSTextStorage) {
    self.textStorage = textStorage
    self.lineMap     = lineTokenMap(textStorage.string, { _string in [] })
    super.init()
  }


  // MARK: -
  // MARK: Font and highlighting

  /// Set a new font in the associated text view.
  ///
  func newFontForTextView(font: NSFont) {

      // Set the font on the associated text view.
    for layoutManager in textStorage.layoutManagers as [NSLayoutManager] {
      if let codeView = layoutManager.firstTextView as? CodeView {
        codeView.font                         = font
        codeView.textGutterView?.needsDisplay = true    // dependent on font size
      }
    }
  }

  /// Set a new theme in the associated text view.
  ///
  func newThemeForTextView(theme: Theme) {

      // Convert the theme to a token dictionary for fast lookup.
    themeDict = themeToDictionary(theme)

      // Set the background colour and re-apply highlighting on the associated text view.
    for layoutManager in textStorage.layoutManagers as [NSLayoutManager] {
      if let codeView = layoutManager.firstTextView as? CodeView {
        codeView.backgroundColor              = theme.background
        codeView.insertionPointColor          = theme.cursor
        codeView.selectedTextAttributes       = [NSBackgroundColorAttributeName: theme.selection]
        codeView.textGutterView?.needsDisplay = true
        codeView.highlight()
      }
    }
  }

  /// Set a tokeniser for highlighting.
  ///
  func enableHighlighting(tokeniser: HighlightingTokeniser) {

      // Register to receive font and theme setting information.
      // NB: We cannot register the code view itself for reporting as `NSTextView`s cannot have weak references.
    ThemesController.sharedThemesController().reportThemeInformation(self,
      fontChangeNotification: curry{ $0.newFontForTextView($1) },
      themeChangeNotification: curry{ $0.newThemeForTextView($1) })

      // Set up highlighting.
    highlightingTokeniser = tokeniser
    lineMap               = lineTokenMap(textStorage.string, tokeniser)
    for layoutManager in textStorage.layoutManagers as [NSLayoutManager] {
      layoutManager.highlight(lineMap)
    }
  }
}


// MARK: -
// MARK: NSTextStorageDelegate protocol

extension CodeStorageDelegate: NSTextStorageDelegate {
  func textStorageDidProcessEditing(notification: NSNotification) {
    let editedRange    = textStorage.editedRange
    let changeInLength = textStorage.changeInLength

    // We need to delay fixing the temporary attributes until after the text storage is done processing the current
    // change.
    dispatch_async(dispatch_get_main_queue(), {
      self.highlightingAfterEditing(editedRange, changeInLength: changeInLength)
    })
  }

  func highlightingAfterEditing(editedRange: NSRange, changeInLength: Int) {

      // FIXME: this should go into SyntaxHighlighting via a call through an extension of NSTextStorage
      //        to pick up the editedRange and changeInLength. (Disentangle from NSTextView to support testing.)
    let oldRange       = editedRange.location ..< (NSMaxRange(editedRange) - changeInLength)
    let string         = textStorage.string
    let editedString   = (string as NSString).substringWithRange(editedRange)
    //    NSLog("edited range = (pos: %i, len: %i); change in length = %i",
    //          editedRange.location, editedRange.length, changeInLength)

      // If the line count changed, we need to recompute the line map and update the gutter.
    let lines          = lineMap.lineRange(oldRange)
    let rescanOffsets  = lineRangeRescanOffsets(lineMap, lines)  // NB: need to use old range, because of old lines map
    let newlineChars   = NSCharacterSet.newlineCharacterSet()
    let didEditNewline = (editedString as NSString).rangeOfCharacterFromSet(newlineChars).location != NSNotFound
                         || lines.endIndex - lines.startIndex > 1
                // FIXME: The above predicate is too coarse. Even if `lines.endIndex - lines.startIndex > 1`, that is ok in that
                //        the number of lines didn't *change*, iff the number of newline characters in the edited string is equal
                //        to `lines.endIndex - lines.startIndex`.
    if didEditNewline {

        // line count changed => compute a completely new line map
      if let tokeniser = highlightingTokeniser {
        lineMap = lineTokenMap(textStorage.string, tokeniser)
      } else {
        lineMap = lineTokenMap(textStorage.string, { _string in [] })
      }

    } else if let tokeniser = highlightingTokeniser {

        // line count stayed the same => may determine lines for rescan with the old (outdated) map
        // FIXME: this is dodgy and really only works, because we only get here in the case where only one line was
        //        modified; we would probably need split `rescanTokenLines` into the fixing up of the start indicies
        //        and the recomputation of the token array, and then. compute the lineRange in the middle...
      let rescanLines = clampRange(extendRange(lines, rescanOffsets), 1...lineMap.lastLine)
      lineMap = rescanTokenLines(lineMap, rescanLines, textStorage.string, tokeniser)

    }

      // For highlighting, we are interested in the new set of lines.
    let editedLines      = lineMap.lineRange(fromNSRange(editedRange))
    let rehighlightLines = clampRange(extendRange(editedLines, rescanOffsets), 1...lineMap.lastLine)
    for layoutManager in textStorage.layoutManagers as [NSLayoutManager] {
      layoutManager.highlight(lineMap, lineRange:rehighlightLines)
      if didEditNewline {
        (layoutManager.firstTextView as? CodeView)?.textGutterView?.needsDisplay = true    // update the line numbering in the gutter
      }
    }
  }
}
