//
//  Box.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation

/// Lift values of any type.
///
final public class Box<T> {
  public let unbox: T
  public init(_ value: T) { self.unbox = value }
}

/// Wrap an object reference such that is only weakly referenced. (This is, e.g., useful to have an array of object
/// references that doesn't keep the referenced objects alive.)
///
/// WARNING: Trying to use a struct, rather than a class here leads swiftc to generate crashing code (XCode 6.1.1)!
public class WeakBox<T: AnyObject> {     // aka Schrödinger Box
  private weak var box: T?
  public var unbox: T? { get { return box } }
  public init(_ value: T) { self.box = value }
}

/// Delayed application of a function to a value, where the application is only computed if the weakly referenced
/// value is still available when the result is demanded.
///
public struct WeakApply<T> {
  private let arg: WeakBox<AnyObject>
  private let fun: AnyObject -> T
  public var unbox: T? { get { return arg.unbox.map(fun) } }
  public init<S: AnyObject>(_ fun: S -> T, _ value: S) {
    self.arg = WeakBox(value)
    self.fun = { fun($0 as S) }
  }
}

// FIXME: this should be in a general module
func curry<R, S, T>(f: (R, S) -> T) -> (R -> S -> T) {
  return { r in { s in f(r, s) } }
}