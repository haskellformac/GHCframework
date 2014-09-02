//
//  SyntaxHighlighting.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Syntax highlighting support.

import Foundation
import GHCKit


/// Constants for the hardcoded theme
// FIXME: must be variable
let highlighBackgroundColour = NSColor(deviceRed: 255/255, green: 252/255, blue: 235/255, alpha: 1)
let commentAttributes        = [NSForegroundColorAttributeName: NSColor(deviceRed: 213/255, green: 134/255, blue: 49/255, alpha: 1)]
let keywordAttributes        = [NSForegroundColorAttributeName: NSColor(deviceRed: 89/255, green: 127/255, blue: 166/255, alpha: 1)]

/// Token types distinguished during syntax highlighting.
///
enum HighlightingTokenKind {
  case LineComment, BlockComment, Other
}

/// Tokens for syntax highlighting.
///
struct HighlightingToken {
  let kind: HighlightingTokenKind
  let span:  SrcSpan

  init (ghcToken: Token) {
    switch ghcToken.kind {
    case .LineComment:  kind = .LineComment
    case .BlockComment: kind = .BlockComment
    case .Other:        kind = .Other
    }
    span = ghcToken.span
  }
}

/// Tokeniser functions take source language stings and turn them into tokens for highlighting.
///
// FIXME: need to have a starting location to tokenise partial programs
typealias HighlightingTokeniser = String -> [HighlightingToken]

extension NSTextView {

  func highlight(tokeniser: HighlightingTokeniser) {
    backgroundColor = highlighBackgroundColour
    layoutManager.highlight(tokeniser)
  }

}

extension NSLayoutManager {

  func highlight(tokeniser: HighlightingTokeniser) {
    let tokens = tokeniser(textStorage.string)

     // FIXME: we really want an efficient mapping from line numbers to character indicies to be able to just map a highlighting function over the token array

    var line: UInt = 1
    var charIndex  = 0 // textStorage.string.startIndex    // beginning of the current line
    var tokenIndex = 0
//    while charIndex < textStorage.string.endIndex && tokenIndex < tokens.endIndex {
    while charIndex < (textStorage.string as NSString).length && tokenIndex < tokens.endIndex {

      var token = tokens[tokenIndex]
      while token.span.start.line == line && tokenIndex < tokens.endIndex {

        if token.span.lines == 0 {

//          let span = advance(charIndex, Int(token.span.start.column))...advance(charIndex, Int(token.span.endColumn))
//          let span = NSRange(location: charIndex + Int(token.span.start.column), length: token.span.endColumn)
          let column = Int(token.span.start.column)
          let span   = NSRange(location: charIndex + column - 1, length: Int(token.span.endColumn) - column)
          switch token.kind {
          case .LineComment, .BlockComment:
          addTemporaryAttributes(commentAttributes, forCharacterRange: span)
          case .Other:
          break
          }

        } else {

        }
        tokenIndex++
        if tokenIndex < tokens.endIndex {
          token = tokens[tokenIndex]
        }
      }
      charIndex = NSMaxRange((textStorage.string as NSString).lineRangeForRange(NSRange(location: charIndex, length: 0)))
      line++
    }
  }
}
