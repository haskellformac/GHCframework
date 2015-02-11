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
public struct Token {
  public let kind: GHCToken
  public let span: SrcSpan

  public init(kind:      GHCToken,
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

extension Token: Equatable {}

public func ==(lhs: Token, rhs: Token) -> Bool {
  return lhs.kind == rhs.kind && lhs.span == rhs.span
}

public typealias DiagnosticsHandler = GHCKit.DiagnosticsHandler


// FIXME: Why '@objc'?
@objc class HaskellSession {

  let ghcInstance: GHCInstance


  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(diagnosticsHandler: DiagnosticsHandler, interactiveWorkingDirectory cwd: String?) {
    ghcInstance = GHCInstance(diagnosticsHandler: diagnosticsHandler, interactiveWorkingDirectory: cwd)
  }


  //MARK: -
  //MARK: Syntax support

  /// Tokenise a string of Haskell code.
  ///
  /// In case of failure, the resulting array is empty, but a diagnostic message is delivered asynchronously as usual.
  ///
  func tokeniseHaskell(text: String, file: String, line: Line, column: Column) -> [Token] {
    return ghcInstance.tokeniseHaskell(text, file: file, line: line, column: column).map{ locatedToken in
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
  func loadModuleFromString(moduleText: String, file: String, importPaths: [String]) -> Bool {
    return ghcInstance.loadModuleFromString(moduleText, file: file, importPaths: importPaths)
  }


  //MARK: -
  //MARK: Code execution

  /// Evaluate an expression in the current evaluation context. The result will either be a 'String' or an object
  /// produced as the result of the evaluation. In case of evaluation errors, the string will simply be empty.
  ///
  /// If evaluation is successful, the second component contains the printed representation of the types of all new
  /// binders.
  ///
  func evalExprFromString(exprText: String, source: String, line: Line) -> (AnyObject, [String]) {
    let resultAndTypes = ghcInstance.evalExprFromString(exprText, source: source, line: line)

    if resultAndTypes.count == 0 { return ("", []) }
    else {
      let types = Array(resultAndTypes[1..<resultAndTypes.endIndex])
      if let typeStrings = types as? [String] {
        return (resultAndTypes[0], typeStrings)
      } else {
        return (resultAndTypes[0], [])
      }
    }
  }
}
