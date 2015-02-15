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
  case Keyword, Keysymbol, VariableWord, VariableSymbol, ConstructorWord, ConstructorSymbol,
       StringLit, CharacterLit, NumberLit, LineComment, BlockComment, Pragma, Other
}

extension HighlightingTokenKind: Printable {
  public var description: String {
    get {
      switch self {
      case .Keyword:           return "Keyword"
      case .Keysymbol:         return "Reserved symbol"
      case .VariableWord:      return "Alphanumeric variable or function"
      case .VariableSymbol:    return "Function operator symbol"
      case .ConstructorWord:   return "Alphanumeric data or type constructor"
      case .ConstructorSymbol: return "Data or type constructor symbol"
      case .StringLit:         return "String constant"
      case .CharacterLit:      return "Character constant"
      case .NumberLit:         return "Numeric constant"
      case .LineComment:       return "Single line or block comment"
      case .BlockComment:      return "Single line or block comment"
      case .Pragma:            return "Compiler pragma"
      case .Other:             return "Default foreground color"
      }
    }
  }
}

/// Map of kinds of highlighting tokens to the foreground colour for tokens of that kind.
///
/// FIXME: Needs to include information concerning underlining, too.
public typealias ThemeDictionary = [HighlightingTokenKind: [NSString: NSColor]]

/// Special highlighting for tab characters.
///
let tabHighlightingAttributes
  = [ NSBackgroundColorAttributeName: NSColor(calibratedRed: 255/255, green: 150/255, blue:  0/255, alpha: 0.5)
    , NSToolTipAttributeName        : "Tab characters should be avoided in Haskell code"]

/// Convert a theme to a dictionary to lookup text attributes by token type.
///
func themeToDictionary(theme: Theme) -> ThemeDictionary {
  return [ .Keyword:           [NSForegroundColorAttributeName: theme.keyword.foreground]
         , .Keysymbol:         [NSForegroundColorAttributeName: theme.keysymbol.foreground]
         , .VariableWord:      [NSForegroundColorAttributeName: theme.varword.foreground]
         , .VariableSymbol:    [NSForegroundColorAttributeName: theme.varsymbol.foreground]
         , .ConstructorWord:   [NSForegroundColorAttributeName: theme.conword.foreground]
         , .ConstructorSymbol: [NSForegroundColorAttributeName: theme.consymbol.foreground]
         , .StringLit:         [NSForegroundColorAttributeName: theme.string.foreground]
         , .CharacterLit:      [NSForegroundColorAttributeName: theme.char.foreground]
         , .NumberLit:         [NSForegroundColorAttributeName: theme.number.foreground]
         , .LineComment:       [NSForegroundColorAttributeName: theme.comment.foreground]
         , .BlockComment:      [NSForegroundColorAttributeName: theme.comment.foreground]
         , .Pragma:            [NSForegroundColorAttributeName: theme.pragma.foreground]
         , .Other:             [NSForegroundColorAttributeName: theme.foreground]
         ]
}

extension Theme {

  /// Update the theme component that corresponds to the given token kind.
  ///
  mutating func setTokenKind(tokenKind: HighlightingTokenKind, colour: NSColor) {
    switch tokenKind {
    case .Keyword:            self.keyword.foreground   = colour
    case .Keysymbol:          self.keysymbol.foreground = colour
    case .VariableWord:       self.varword.foreground   = colour
    case .VariableSymbol:     self.varsymbol.foreground = colour
    case .ConstructorWord:    self.conword.foreground   = colour
    case .ConstructorSymbol:  self.consymbol.foreground = colour
    case .StringLit:          self.string.foreground    = colour
    case .CharacterLit:       self.char.foreground      = colour
    case .NumberLit:          self.number.foreground    = colour
    case .LineComment:        self.comment.foreground   = colour
    case .BlockComment:       self.comment.foreground   = colour
    case .Pragma:             self.pragma.foreground    = colour
    case .Other:              self.foreground           = colour
    }
  }
}


/// Tokens for syntax highlighting.
///
public struct HighlightingToken {
  public let kind: HighlightingTokenKind
  public let span: SrcSpan

  public init (ghcToken: Token) {
    switch ghcToken.kind {
    case .As:                 kind = .Keyword
    case .Case:               kind = .Keyword
    case .Class:              kind = .Keyword
    case .Data:               kind = .Keyword
    case .Default:            kind = .Keyword
    case .Deriving:           kind = .Keyword
    case .Do:                 kind = .Keyword
    case .Else:               kind = .Keyword
    case .Hiding:             kind = .Keyword
    case .If:                 kind = .Keyword
    case .Import:             kind = .Keyword
    case .In:                 kind = .Keyword
    case .Infix:              kind = .Keyword
    case .Infixl:             kind = .Keyword
    case .Infixr:             kind = .Keyword
    case .Instance:           kind = .Keyword
    case .Let:                kind = .Keyword
    case .Module:             kind = .Keyword
    case .Newtype:            kind = .Keyword
    case .Of:                 kind = .Keyword
    case .Qualified:          kind = .Keyword
    case .Then:               kind = .Keyword
    case .Type:               kind = .Keyword
    case .Where:              kind = .Keyword
    case .Forall:             kind = .Keyword
    case .Foreign:            kind = .Keyword
    case .Export:             kind = .Keyword
    case .Label:              kind = .Keyword
    case .Dynamic:            kind = .Keyword
    case .Safe:               kind = .Keyword
    case .Interruptible:      kind = .Keyword
    case .Unsafe:             kind = .Keyword
    case .Stdcallconv:        kind = .Keyword
    case .Ccallconv:          kind = .Keyword
    case .Capiconv:           kind = .Keyword
    case .Primcallconv:       kind = .Keyword
    case .Javascriptcallconv: kind = .Keyword
    case .Mdo:                kind = .Keyword
    case .Family:             kind = .Keyword
    case .Role:               kind = .Keyword
    case .Group:              kind = .Keyword
    case .By:                 kind = .Keyword
    case .Using:              kind = .Keyword
    case .Pattern:            kind = .Keyword
    case .Ctype:              kind = .Keyword

    case .Dotdot:             kind = .Keysymbol
    case .Colon:              kind = .Keysymbol
    case .Dcolon:             kind = .Keysymbol
    case .Equal:              kind = .Keysymbol
    case .Lam:                kind = .Keysymbol
    case .Lcase:              kind = .Keysymbol
    case .Vbar:               kind = .Keysymbol
    case .Larrow:             kind = .Keysymbol
    case .Rarrow:             kind = .Keysymbol
    case .At:                 kind = .Keysymbol
    case .Tilde:              kind = .Keysymbol
    case .Tildehsh:           kind = .Keysymbol
    case .Darrow:             kind = .Keysymbol
    case .Minus:              kind = .Keysymbol
    case .Bang:               kind = .Keysymbol
    case .Star:               kind = .Keysymbol
    case .Dot:                kind = .Keysymbol
    case .Biglam:             kind = .Keysymbol
    case .Ocurly:             kind = .Keysymbol
    case .Ccurly:             kind = .Keysymbol
    case .Vocurly:            kind = .Keysymbol
    case .Vccurly:            kind = .Keysymbol
    case .Obrack:             kind = .Keysymbol
    case .Opabrack:           kind = .Keysymbol
    case .Cpabrack:           kind = .Keysymbol
    case .Cbrack:             kind = .Keysymbol
    case .Oparen:             kind = .Keysymbol
    case .Cparen:             kind = .Keysymbol
    case .Oubxparen:          kind = .Keysymbol
    case .Cubxparen:          kind = .Keysymbol
    case .Semi:               kind = .Keysymbol
    case .Comma:              kind = .Keysymbol
    case .Underscore:         kind = .Keysymbol
    case .Backquote:          kind = .Keysymbol
    case .SimpleQuote:        kind = .Keysymbol

    case .Varsym:             kind = .VariableSymbol
    case .Consym:             kind = .ConstructorSymbol
    case .Varid:              kind = .VariableWord
    case .Conid:              kind = .ConstructorWord
    case .Qvarsym:            kind = .VariableSymbol
    case .Qconsym:            kind = .ConstructorSymbol
    case .Qvarid:             kind = .VariableWord
    case .Qconid:             kind = .ConstructorWord

    case .Integer:            kind = .NumberLit
    case .Rational:           kind = .NumberLit

    case .Char:               kind = .CharacterLit
    case .String:             kind = .StringLit
    case .LineComment:        kind = .LineComment
    case .BlockComment:       kind = .BlockComment
    case .Other:              kind = .Other
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
public typealias HighlightingTokeniser = (Line, Column, String) -> [HighlightingToken]

/// Initialise a line token map from a string.
///
public func lineTokenMap(string: String, tokeniser: HighlightingTokeniser) -> LineTokenMap {
  var lineMap: LineTokenMap = StringLineMap(string: string)
  lineMap.addLineInfo(tokensForLines(tokeniser(1, 1, string)))
  return lineMap
}

/// Compute the line assignment for an array of tokens.
///
private func tokensForLines(tokens: [HighlightingToken]) -> [(Line, HighlightingToken)] {

  // Compute the tokens for every line.
  var lineInfo: [(Line, HighlightingToken)] = []
  for token in tokens {
    for offset in 0..<token.span.lines {
      let lineToken: (Line, HighlightingToken) = (token.span.start.line + offset, token)
      lineInfo.append(lineToken)
    }
  }
  return lineInfo
}

/// For the given line range, determine how many preceeding or succeeding lines are part of multi-line tokens, and
/// hence, need to be rescanned for re-tokenisation.
///
/// The current implementation is a conservative approximation as it extends the range by the number of extra lines that
/// the token has *independent* of which lines within the multiline token is at the border of the original line range.
///
public func lineRangeRescanOffsets(lineTokenMap: LineTokenMap, lines: Range<Line>) -> (UInt, UInt) {
  if lines.isEmpty { return (0, 0) }

  let preOffset: UInt = {
    if let firstToken = lineTokenMap.infoOfLine(lines.startIndex).first {
      if firstToken.span.lines > 1 {
        return firstToken.span.lines - 1
      }
    }
    return 0
  }()
  let sucOffset: UInt = {
    if let lastToken = lineTokenMap.infoOfLine(lines.endIndex - 1).last {
      if lastToken.span.lines > 1 {
        return lastToken.span.lines - 1
      }
    }
    return 0
    }()
  return (preOffset, sucOffset)
}

/// Update the given token map by re-running the tokeniser on the given range of lines and fixing up the start indicies
/// (and the last line end index) for all rescan lines and all following lines.
///
/// PRECONDITION: This function expects that the number of lines in the old and new line token map are the *same*.
///
public func rescanTokenLines(lineMap: LineTokenMap,
                         rescanLines: Range<Line>,
                              string: String,
                           tokeniser: HighlightingTokeniser) -> LineTokenMap
{
  if rescanLines.isEmpty { return lineMap }

  var newLineMap = lineMap
  if let startIndex = lineMap.startOfLine(rescanLines.startIndex) {

      // First, fix the start indicies in the line map (for edited lines and all following lines).
    var idx = startIndex
    for line in rescanLines {                                       // fix rescan lines
      newLineMap.setStartOfLine(line, startIndex: idx)
      idx = NSMaxRange((string as NSString).lineRangeForRange(NSRange(location: idx, length: 0)))
    }
    let oldEndIndex    = lineMap.endOfLine(rescanLines.endIndex - 1)
    let newEndIndex    = idx
    let changeInLength = newEndIndex - oldEndIndex

    newLineMap.setStartOfLine(0, startIndex: string.utf16.endIndex) // special case of the end of the string

    for line in rescanLines.endIndex..<(lineMap.lastLine + 1) {     // fix all lines after the rescan lines
      newLineMap.setStartOfLine(line, startIndex: advance(lineMap.startOfLine(line)!, changeInLength))
    }

      // Second, we update the token information for all affected lines.
    let rescanString = (string as NSString).substringWithRange(toNSRange(startIndex..<newEndIndex))
    newLineMap.replaceLineInfo(rescanLines.map{ ($0, []) })       // FIXME: this and next line might be nicer combined
    newLineMap.addLineInfo(tokensForLines(tokeniser(rescanLines.startIndex, 1, rescanString)))
    return newLineMap

  } else { return lineMap }
}

/// Computes an array of tokens (and their source span) for one line from a `LineTokenMap`.
///
public func tokensAtLine(lineTokenMap: LineTokenMap)(line: Line) -> [(HighlightingToken, Range<Int>)] {
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

/// Compute the tokens occuring in this character range at the underlying text storage. If even a single character of
/// a token is in the range, we regard the token to be in the range.
///
public func tokens(lineTokenMap: LineTokenMap, inRange range: Range<Int>) -> [HighlightingToken] {

  func tokenIsInRange(tokenWithRange: (HighlightingToken, Range<Int>)) -> Bool {
    return tokenWithRange.1.startIndex < range.endIndex && tokenWithRange.1.endIndex > range.startIndex
  }

  let lineRange = lineTokenMap.lineRange(range)
  return [].join(lineRange.map(tokensAtLine(lineTokenMap))).filter(tokenIsInRange).map{$0.0}
}

extension CodeView {

  func enableHighlighting(tokeniser: HighlightingTokeniser) {
    layoutManager?.enableHighlighting(tokeniser)
  }

  /// Perform syntax highlighting for the given line range.
  ///
  func highlight(lineRange: Range<Line>) {
    if let lineMap = self.lineMap { layoutManager?.highlight(lineMap, lineRange: lineRange) }
  }

  /// Perform syntax highlighting for the entire text.
  ///
  func highlight() {
    if let lineMap = self.lineMap { layoutManager?.highlight(lineMap) }
  }
}

extension NSLayoutManager {

  func enableHighlighting(tokeniser: HighlightingTokeniser) {
    (textStorage?.delegate as? CodeStorageDelegate)?.enableHighlighting(tokeniser)
  }

  /// Perform syntax highlighting for all lines in the line map.
  ///
  func highlight(lineTokenMap: LineTokenMap) {
    highlight(lineTokenMap, lineRange: 1...lineTokenMap.lastLine)
  }

  /// Perform syntax highlighting for the given range of lines in the line map.
  ///
  func highlight(lineTokenMap: LineTokenMap, lineRange: Range<Line>) {
    if lineRange.isEmpty { return }

      // Remove any existing temporary attributes in the entire range and highlight tabs.
    if let start = lineTokenMap.startOfLine(lineRange.startIndex) {

      let end = lineTokenMap.endOfLine(lineRange.endIndex - 1)
      setTemporaryAttributes([:], forCharacterRange: toNSRange(start..<end))

        // Mark all tab characters.
      let tabCharacterSet: NSCharacterSet = NSCharacterSet(charactersInString: "\t")
      let string:          NSString       = textStorage!.string
      var charIndex:       Int            = start
      while (charIndex != NSNotFound && charIndex < end) {
        let searchRange = NSRange(location: charIndex, length: end - charIndex)
        let foundRange  = string.rangeOfCharacterFromSet(tabCharacterSet, options: nil, range: searchRange)
        if foundRange.location != NSNotFound {

          addTemporaryAttributes(tabHighlightingAttributes, forCharacterRange: foundRange)
          charIndex = NSMaxRange(foundRange)

        } else { charIndex = NSNotFound }
      }
    }

      // Apply highlighting to all tokens in the affected range.
    for (token, span) in [].join(lineRange.map(tokensAtLine(lineTokenMap))) {
      if let attributes = (textStorage?.delegate as? CodeStorageDelegate)?.themeDict[token.kind] {
        addTemporaryAttributes(attributes, forCharacterRange: toNSRange(span))
      }
    }
  }
}
