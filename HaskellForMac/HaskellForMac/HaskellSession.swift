//
//  HaskellSession.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 1/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Each instance of this class represents a GHC session. This class is a view model interfacing the Haskell-side model.

//import Foundation

import GHCKit

@objc class HaskellSession {

  let ghcInstance: GHCInstance


  //MARK: -
  //MARK: Initialisation and deinitialisation

  init(diagnosticsHandler: GHCKit.DiagnosticsHandler) {
    ghcInstance = GHCInstance(diagnosticsHandler: diagnosticsHandler)
  }


  //MARK: -
  //MARK: Code loading

  /// Load a single module and make it the current evaluation context.
  //
  func loadModuleFromString(moduleText: String) -> String {
    return ghcInstance.loadModuleFromString(moduleText)
  }


  //MARK: -
  //MARK: Code execution

  /// Evaluate an expression in the current evaluation context.
  //
  func evalExprFromString(exprText: String) -> String {
    return ghcInstance.evalExprFromString(exprText)
  }
}
