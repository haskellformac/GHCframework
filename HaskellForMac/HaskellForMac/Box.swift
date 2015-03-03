//
//  Box.swift
//  TacticalBase
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

/// Mutable reference to values of any type.
///
final public class Ref<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

/// Wrap an object reference such that is only weakly referenced. (This is, e.g., useful to have an array of object
/// references that doesn't keep the referenced objects alive.)
///
/// WARNING: Trying to use a struct, rather than a class here leads swiftc to generate crashing code (XCode 6.1.1)!
final public class WeakBox<T: AnyObject> {     // aka Schrödinger's Box
    private weak var box: T?  // RADAR #20020634
//  private weak var box: AnyObject?
//  public var unbox: T? { get { if let v: AnyObject = box { if let w = v as? T { return w } else { NSLog("1"); return nil } } else { NSLog("2"); return nil } } }
//  public var unbox: T? { get { if let v: AnyObject = box { if let w = box as? T { return w } else { return nil } } else { return nil } } }
  //  public var unbox: T? { get { return box as! T? } }  // leads to a runtime errors when performing the cast in some cases
    public var unbox: T? { get { return box } }
//  public init(_ value: T) { self.box = value as AnyObject }
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
    self.fun = { fun($0 as! S) }
  }
}
