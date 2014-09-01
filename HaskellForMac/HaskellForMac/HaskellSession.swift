//
//  HaskellSession.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 1/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Each instance of this class represents a GHC session. This class is a view model interfacing the Haskell-side model.

import Foundation
import GHCKit


/// Localised token type
///
struct Token {
  let kind: GHCToken
  let span: SrcSpan

  init(kind:      GHCToken,
       filename:  String,
       line:      UInt,
       column:    UInt,
       lines:     UInt,
       endColumn: UInt)
  {
    self.kind = kind
    self.span = SrcSpan(start: SrcLoc(file: filename, line: line, column: column),
                        lines: lines,
                        endColumn: endColumn)
  }
}

@objc class HaskellSession {

  let ghcInstance: GHCInstance


  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(diagnosticsHandler: GHCKit.DiagnosticsHandler) {
    ghcInstance = GHCInstance(diagnosticsHandler: diagnosticsHandler)
  }


  //MARK: -
  //MARK: Syntax support

  /// Tokenise a string of Haskell code.
  ///
  /// In case of failure, the resulting array is empty, but a diagnostic message is delivered asynchronously as usual.
  ///
  func tokeniseHaskell(text: String, file: String) -> [Token] {
    return ghcInstance.tokeniseHaskell(text, file: file).map{ locatedToken in
      let tok = locatedToken as GHCLocatedToken
      return Token(kind: locatedToken.token,
                   filename: file,
                   line: tok.line,
                   column: tok.column,
                   lines: tok.lines,
                   endColumn: tok.endColumn)
    }
  }


  //MARK: -
  //MARK: Code loading

  /// Load a single module and make it the current evaluation context.
  ///
  func loadModuleFromString(moduleText: String, file: String) -> Bool {
    return ghcInstance.loadModuleFromString(moduleText, file: file)
  }


  //MARK: -
  //MARK: Code execution

  /// Evaluate an expression in the current evaluation context.
  ///
  func evalExprFromString(exprText: String, source: String, line: Line) -> String {
    return ghcInstance.evalExprFromString(exprText, source: source, line: line)
  }
}
