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


/// Token types distinguished during syntax highlighting.
///
enum HighlightingTokenKind {
  case Constructor, String, Number, Keyword, LineComment, BlockComment, Other
}

/// Constants for the hardcoded theme
// FIXME: must be variable
let highlighBackgroundColour = NSColor(calibratedRed: 255/255, green: 252/255, blue: 235/255, alpha: 1)
let constructorAttributes    = [NSForegroundColorAttributeName: NSColor(calibratedRed: 180/255, green:  69/255, blue:   0/255, alpha: 1)]
let stringAttributes         = [NSForegroundColorAttributeName: NSColor(calibratedRed: 223/255, green:   7/255, blue:   0/255, alpha: 1)]
let numberAttributes         = [NSForegroundColorAttributeName: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1)]
let keywordAttributes        = [NSForegroundColorAttributeName: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1)]
let commentAttributes        = [NSForegroundColorAttributeName: NSColor(calibratedRed: 195/255, green: 116/255, blue:  28/255, alpha: 1)]

/// Theme = dictionary to look up attributes for a given token type.
///
let theme: [HighlightingTokenKind: [NSString: NSColor]] = [ .Constructor:  constructorAttributes
                                                          , .String:       stringAttributes
                                                          , .Number:       numberAttributes
                                                          , .Keyword:      keywordAttributes
                                                          , .LineComment:  commentAttributes
                                                          , .BlockComment: commentAttributes
                                                          ]

/// Tokens for syntax highlighting.
///
struct HighlightingToken {
  let kind: HighlightingTokenKind
  let span:  SrcSpan

  init (ghcToken: Token) {
    switch ghcToken.kind {
    case .As:           kind = .Keyword
    case .Case:         kind = .Keyword
    case .Class:        kind = .Keyword
    case .Data:         kind = .Keyword

    case .Dotdot:       kind = .Keyword
    case .Colon:        kind = .Keyword
    case .Dcolon:       kind = .Keyword
    case .Equal:        kind = .Keyword

    case .Varsym:       kind = .Other
    case .Consym:       kind = .Constructor
    case .Varid:        kind = .Other
    case .Conid:        kind = .Constructor
    case .Qvarsym:      kind = .Other
    case .Qconsym:      kind = .Constructor
    case .Qvarid:       kind = .Other
    case .Qconid:       kind = .Constructor

    case .Integer:      kind = .Number
    case .Rational:     kind = .Number

    case .Char:         kind = .String
    case .String:       kind = .String
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
          if let attributes = theme[token.kind] {
            addTemporaryAttributes(attributes, forCharacterRange: span)
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
