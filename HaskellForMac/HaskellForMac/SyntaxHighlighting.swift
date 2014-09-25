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
public enum HighlightingTokenKind {
  case Constructor, String, Number, Keyword, LineComment, BlockComment, Other
}

/// Constants for the hardcoded theme
// FIXME: must be variable
let highlighBackgroundColour = NSColor(calibratedRed: 255/255, green: 252/255, blue: 235/255, alpha: 1)!
let constructorAttributes    = [NSForegroundColorAttributeName: NSColor(calibratedRed: 180/255, green:  69/255, blue:   0/255, alpha: 1)!]
let stringAttributes         = [NSForegroundColorAttributeName: NSColor(calibratedRed: 223/255, green:   7/255, blue:   0/255, alpha: 1)!]
let numberAttributes         = [NSForegroundColorAttributeName: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1)!]
let keywordAttributes        = [NSForegroundColorAttributeName: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1)!]
let commentAttributes        = [NSForegroundColorAttributeName: NSColor(calibratedRed: 195/255, green: 116/255, blue:  28/255, alpha: 1)!]

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
public struct HighlightingToken {
  public let kind: HighlightingTokenKind
  public let span: SrcSpan

  public init (ghcToken: Token) {
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

extension HighlightingToken: Equatable {}

public func ==(lhs: HighlightingToken, rhs: HighlightingToken) -> Bool {
  return lhs.kind == rhs.kind && lhs.span == rhs.span
}

/// Map from line numbers to pairs of character index (where the line starts) and tokens on the line.
///
/// Tokens are in column order. If a token spans multiple lines, it occurs as the last token on its first line and as
/// the first (and possibly only) token on all of its subsqeuent lines.
///
public typealias LineTokenMap = StringLineMap<HighlightingToken>

/// Tokeniser functions take source language stings and turn them into tokens for highlighting.
///
// FIXME: need to have a starting location to tokenise partial programs
public typealias HighlightingTokeniser = String -> [HighlightingToken]

/// Initialise a line token map from a string.
///
public func lineTokenMap(string: String, tokeniser: HighlightingTokeniser) -> LineTokenMap {
  var lineMap: LineTokenMap = StringLineMap(string: string)
  let tokens                = tokeniser(string)

    // Compute the tokens for every line.
  var lineInfo: [(Line, HighlightingToken)] = []
  for token in tokens {
    for offset in 0..<token.span.lines {
      let lineToken: (Line, HighlightingToken) = (token.span.start.line + offset, token)
      lineInfo.append(lineToken)
    }
  }

  lineMap.addLineInfo(lineInfo)
  return lineMap
}

/// Computes an array of tokens (and their source span in string indicies) for a range of lines from a `LineTokenMap`.
///
public func tokensWithSpan(lineTokenMap: LineTokenMap)(atLine line: Line) -> [(HighlightingToken, Range<String.Index>)] {
  if let index = lineTokenMap.startOfLine(line) {
    let tokens = lineTokenMap.infoOfLine(line)
    return map(tokens){ token in
      let endLine = token.span.start.line + token.span.lines - 1
      let start   = line == token.span.start.line
                    ? advance(index, Int(token.span.start.column) - 1)   // token starts on this line
                    : index                                              // token started on a previous line
      let end     = line == endLine
                    ? advance(index, Int(token.span.endColumn) - 1)      // token ends on this line
                    : lineTokenMap.endOfLine(line)                       // token ends on a subsequent line
      return (token, start..<end)
    }
  } else {
    return []
  }
}

extension NSTextView {

  func highlight(lineTokenMap: LineTokenMap, lineRange: Range<Line>) {
    backgroundColor = highlighBackgroundColour
    layoutManager.highlight(lineTokenMap, lineRange: lineRange)
  }

  func highlight(lineTokenMap: LineTokenMap) {
    return highlight(lineTokenMap, lineRange: 1..<lineTokenMap.lastLine)
  }

}

extension NSLayoutManager {

  func highlight(lineTokenMap: LineTokenMap, lineRange: Range<Line>) {
    let string = textStorage.string
    for (token, span) in [].join(lineRange.map(tokensWithSpan(lineTokenMap))) {
      if let attributes = theme[token.kind] {
        let location = string[string.startIndex..<span.startIndex].utf16Count     // FIXME: is this efficient????
        let length   = string[span].utf16Count
        addTemporaryAttributes(attributes, forCharacterRange: NSRange(location: location, length: length))
      }
    }

/*
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
    */
  }
}
