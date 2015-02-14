//
//  Functions.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 14/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation

infix operator .. { associativity right }

public func ..<R, S, T>(f: S -> T, g: R -> S) -> (R -> T) {
  return { f(g($0)) }
}

public func curry<R, S, T>(f: (R, S) -> T) -> (R -> S -> T) {
  return { r in { s in f(r, s) } }
}

public func flip<R, S, T>(f: (R, S) -> T) -> (S -> R -> T) {
  return { s in { r in f(r, s) } }
}