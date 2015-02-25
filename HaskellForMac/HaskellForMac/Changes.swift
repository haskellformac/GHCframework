//
//  Changes.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 25/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Generic support to keep track of changes and values changing over time.

import Foundation


/// A stream of changes that announces change values of type `Change` to a set of observers.
///
public class Changes<Change> {
  public typealias Observer = Change -> ()

  /// Registered observers
  ///
  /// NB: Observers are bound to objects tracked by weak references, they may go at any time, which implicitly
  ///     unregisters the corresponding observer.
  ///
  private var observers: [WeakApply<Observer>]  = []

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  public func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Observer) {
    observers.append(WeakApply(observer, context))
  }

  /// Announce a change to all observers.
  ///
  public func announce(change: Change)
  {
    for observer in observers { if let callback = observer.unbox { callback(change) } }

      // Prune stale observers.
    observers = observers.filter{ $0.unbox != nil }
  }
}

/// Variables represent a time series of changing values of type `T`. Observers are informed of a variable's value
/// from the moment they start to observe until their associated context object disappears.
///
public class Variable<T> {
  typealias ValueObserver = Changes<T>.Observer

  var value: T
    { didSet { valueChanges.announce(value) } }

  private var valueChanges: Changes<T> = Changes()

  init(initialValue: T) { self.value = initialValue }

  /// Register a value observer, which is immediately called with the current value of the variable.
  ///
  public func observeValuesWithContext<Context: AnyObject>(context: Context, observer: Context -> ValueObserver) {
    valueChanges.observeWithContext(context, observer: observer)
    observer(context)(value)
  }
}

extension Changes {

  /// Transform a stream of changes to a derived stream of changes.
  ///
  /// The derived stream will cease to announce changes if the last reference to it has been dropped. (That does not
  /// mean that it hasn't got any observers anymore, but that no other object keeps a strong reference to the stream
  /// of changes itself.)
  ///
  func map<NewChange>(f: Change -> NewChange) -> Changes<NewChange> {

    let newChanges = Changes<NewChange>()
    self.observeWithContext(newChanges,
                            observer: curry{ (newChangesContext, change) in newChangesContext.announce(f(change)) })
    return newChanges
  }
}
