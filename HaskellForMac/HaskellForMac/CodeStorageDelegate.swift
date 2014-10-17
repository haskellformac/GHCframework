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

  /// Map from line numbers to pairs of character index (where the line starts) and tokens on that line.
  ///
  /// This value is mutable as it changes while the underlying text storage is being edited.
  ///
  var lineMap: LineTokenMap

  init(textStorage: NSTextStorage) {
    self.textStorage = textStorage
    self.lineMap     = lineTokenMap(textStorage.string, { _string in [] })
  }


  // MARK: -
  // MARK: Highlighting

  /// Set a tokeniser to for highlighting.
  ///
  func enableHighlighting(tokeniser: HighlightingTokeniser) {
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
      let rescanLines = clampRange(extendRange(lines, rescanOffsets), 1..<lineMap.lastLine)
      lineMap = rescanTokenLines(lineMap, rescanLines, textStorage.string, tokeniser)

    }

      // For highlighting, we interested in the new set of lines.
    let editedLines      = lineMap.lineRange(fromNSRange(editedRange))
    let rehighlightLines = clampRange(extendRange(editedLines, rescanOffsets), 1..<lineMap.lastLine)
    for layoutManager in textStorage.layoutManagers as [NSLayoutManager] {
      layoutManager.highlight(lineMap, lineRange:rehighlightLines)
      if didEditNewline {
        layoutManager.firstTextView?.enclosingScrollView?.verticalRulerView!.needsDisplay = true    // update the line numbering in the gutter
      }
    }
  }
}
