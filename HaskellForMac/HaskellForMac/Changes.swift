//
//  Changes.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 25/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Generic support to keep track of changes and values changing over time.

import Foundation


/// Abstract interface to an observable stream of changes over time.
///
public protocol Observable {

  /// The changing value.
  ///
  typealias Value

  /// Registered observers
  ///
  /// NB: Observers are bound to objects tracked by weak references, they may go at any time, which implicitly
  ///     unregisters the corresponding observer.
  ///
  typealias Observer = Value -> ()

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The observer will be called on the same thread where a new value is announced.
  ///
  func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Value -> ())
  //  func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Observer)
  // ^^^ This signature would be nice, but gets the Swift compiler (Xcode 6.1.1) into trouble as it cannot look through
  //     `Observer` when using this protocol.

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The observer will be called on the specified execution queue or, by the default, the main dispatch queue.
  ///
  func asyncObserveWithContext<Context: AnyObject>(context: Context,
                                                   onQueue queue: dispatch_queue_t?,
                                                   observer: Context -> Value -> ())
  //                                                   observer: Context -> Observer)
  // FIXME: See explanation above.
}


// MARK: -
// MARK: Observable classes

/// A stream of changes that announces change values of type `Change` to a set of observers.
///
public class Changes<Change>: Observable {
  typealias Value = Change

  public typealias Observer = Change -> ()

  /// Registered observers
  ///
  /// NB: Observers are bound to objects tracked by weak references, they may go at any time, which implicitly
  ///     unregisters the corresponding observer.
  ///
  private var observers: [WeakApply<Observer>]  = []

  /// Announce a change to all observers.
  ///
  public func announce(change: Change)
  {
    for observer in observers { if let callback = observer.unbox { callback(change) } }

      // Prune stale observers.
    observers = observers.filter{ $0.unbox != nil }
  }

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The observer will be called on the same thread as the change announcement.
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
                                                          onQueue queue: dispatch_queue_t? = dispatch_get_main_queue(),
                                                          observer: Context -> Observer)
  {
    let dispatchedObserver = curry{ context, change in dispatch_async(queue){ observer(context)(change) } }
    observers.append(WeakApply(dispatchedObserver, context))
  }
}

/// A stream of timer ticks.
///
public class TimerChanges: Observable {
         typealias Value    = NSDate
  public typealias Observer = NSDate -> ()

  /// The weak reference is fine as the timer will be strongly referenced from the run loop for the duration of its life.
  ///
  private weak var timer:   NSTimer?

  private      let changes: Changes<NSDate>

  /// Create a time stream with a given tick interval and tolerance.
  ///
  init(every: NSTimeInterval, tolerance: NSTimeInterval) {
    self.changes = Changes()
    self.timer   = NSTimer.scheduledTimerWithTimeInterval(every,
                                                          target: self,
                                                          selector: "timerFired:",
                                                          userInfo: nil,
                                                          repeats: true)
    self.timer?.tolerance = tolerance
  }

  /// Create a timer stream without any tolerance.
  ///
  convenience init(every: NSTimeInterval) { self.init(every: every, tolerance: 0) }

  deinit { timer?.invalidate() }

  private func timerFired(timer: NSTimer) { changes.announce(NSDate()) }

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The time will be scheduled on the current run loop in default mode.
  ///
  public func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Observer) {
    changes.observeWithContext(context, observer: observer)
  }

  /// Register an observer together with a context object whose lifetime determines the duration of the observation.
  ///
  /// The context object is only stored using a weak reference.
  ///
  /// The time will be scheduled on the current run loop in default mode, but observer will be notified asynchronously
  /// on the chosen queue, or the main queue if no other queue specified.
  ///
  public func asyncObserveWithContext<Context: AnyObject>(context: Context,
    onQueue queue: dispatch_queue_t? = dispatch_get_main_queue(),
    observer: Context -> Observer)
  {
    changes.asyncObserveWithContext(context, onQueue: queue, observer: observer)
  }
}

/// Variables represent a time series of changing values of type `T`. Observers are informed of a variable's value
/// from the moment they start to observe until their associated context object disappears.
///
public class Variable<T>: Observable {
  typealias Value = T

  public typealias Observer = Changes<T>.Observer

  var value: T
    { didSet { valueChanges.announce(value) } }

  private var valueChanges: Changes<T> = Changes()

  init(initialValue: T) { self.value = initialValue }

  /// Register a value observer, which is immediately called with the current value of the variable.
  ///
  /// The context object only gets a weak reference and determines the lifetime of the observer.
  ///
  /// The observer will be called on the same thread as the value update.
  ///
  public func observeWithContext<Context: AnyObject>(context: Context, observer: Context -> Observer) {
    valueChanges.observeWithContext(context, observer: observer)
    observer(context)(value)
  }

  /// Register a value observer, which is immediately called with the current value of the variable.
  ///
  /// The context object only gets a weak reference and determines the lifetime of the observer.
  ///
  /// The observer will be called on the specified execution queue or, by the default, the main dispatch queue.
  ///
  public func asyncObserveWithContext<Context: AnyObject>(context: Context,
                                                          onQueue queue: dispatch_queue_t? = dispatch_get_main_queue(),
                                                          observer: Context -> Observer)
  {
    valueChanges.asyncObserveWithContext(context, onQueue: queue, observer: observer)
    observer(context)(value)
  }
}


// MARK: -
// MARK: Transforming observations

/// Transform a stream of observations to a derived stream of changes.
///
/// The derived stream will cease to announce changes if the last reference to it has been dropped. (That does not
/// mean that it hasn't got any observers anymore, but that no other object keeps a strong reference to the stream
/// of changes itself.)
///
public func map<Observed: Observable, Change>(source: Observed, f: Observed.Value -> Change) -> Changes<Change> {

  let changes = Changes<Change>()
  source.observeWithContext(changes,
                            observer: curry{ changesContext, change in changesContext.announce(f(change)) })
  return changes
}

// FIXME: This should go into a general module.
public enum Either<S, T> {
  case Left(Box<S>)
  case Right(Box<T>)
}

/// Merge to observation streams into one.
///
/// The derived stream will cease to announce changes if the last reference to it has been dropped. (That does not
/// mean that it hasn't got any observers anymore, but that no other object keeps a strong reference to the stream
/// of changes itself.)
///
public func merge<ObservedLeft: Observable, ObservedRight: Observable>(left: ObservedLeft, right: ObservedRight)
  -> Changes<Either<ObservedLeft.Value, ObservedRight.Value>>
{
  typealias Change = Either<ObservedLeft.Value, ObservedRight.Value>

  let changes = Changes<Change>()
  left.observeWithContext(changes,
                          observer: curry{ changesContext, change in
                                             let leftChange: Change = .Left(Box(change))
                                             changesContext.announce(leftChange) })
  right.observeWithContext(changes,
                           observer: curry{ changesContext, change in
                                              let rightChange: Change = .Right(Box(change))
                                              changesContext.announce(rightChange) })
  return changes
}

/// A stream transducer transforming a stream of observations into a stream of changes. The transducer has internal
/// state dependent on the observations it has already made.
///
/// The derived stream will cease to announce changes if the last reference to it has been dropped. (That does not
/// mean that it hasn't got any observers anymore, but that no other object keeps a strong reference to the stream
/// of changes itself.)
///
public func transduce<Observed: Observable, Change, State>(source: Observed,
                                                           state: State,
                                                           f: (State, Observed.Value) -> (State, Change))
                                                           -> Changes<Change>
{
  let changes  = Changes<Change>()
  let stateRef = Ref(state)   // the mutable transducer state

  func observer(changesContext: Changes<Change>, value: Observed.Value) {
    let (newState, change) = f(stateRef.value, value)
    stateRef.value = newState
    changesContext.announce(change)
  }

  source.observeWithContext(changes, observer: curry(observer))
  return changes
}
