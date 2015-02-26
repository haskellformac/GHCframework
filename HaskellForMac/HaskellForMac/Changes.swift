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
  /// The context object is only stored using a weak reference.
  ///
  /// The observer will be called on the same execution queue as the change announcement.
  ///
  public func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Observer) {
    observers.append(WeakApply(observer, context))
  }

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The observer will be called on the specified execution queue or, by the default, the main dispatch queue.
  ///
  public func asyncObserveWithContext<Context: AnyObject>(context: Context,
                                                          onQueue queue: dispatch_queue_t = dispatch_get_main_queue(),
                                                          observer: Context -> Observer)
  {
    // FIXME: save the dispatch call if the current queue is already the one we ought to dispatch on
    //        maybe make the queue arg into an option with nil meaning take the current queue and unify with other function
    let dispatchedObserver = curry{ context, change in dispatch_async(queue){ observer(context)(change) } }
    observers.append(WeakApply(dispatchedObserver, context))
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
  /// The context object only gets a weak reference and determines the lifetime of the observer.
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
