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

typealias HighlightingTokeniser = String -> [HighlightingToken]

extension NSTextView {

//  func highlight() {
//    textStorage.highlight
//  }

}