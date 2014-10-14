//
//  Error.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


/// The Swift compiler should do this automatically...
final public class Box<T> {
  public let unbox: T
  public init(_ value: T) { self.unbox = value }
}

/// A value that we might fail to obtain, in which case we get an error object instead.
///
enum ErrorOr<ResultType> {
  case Error(NSError)
  case Result(Box<ResultType>)
}

func result<ResultType>(result: ResultType) -> ErrorOr<ResultType> {
  return .Result(Box(result))
}

func error<ResultType>(reason: String) -> ErrorOr<ResultType> {
  return .Error(NSError(domain: "HfM Error", code: 0, userInfo: [NSLocalizedDescriptionKey: reason]))
}

extension ErrorOr {
  init(optionalError: String?, result value: ResultType) {
    if let errorMsg = optionalError {
      self = error(errorMsg)
    } else {
      self = result(value)
    }
  }
}

extension Optional {
  init(errorOrResult: ErrorOr<T>) {
    switch errorOrResult {
    case .Error(let _):      self = .None
    case .Result(let value): self = .Some(value.unbox)
    }
  }
}

func ==<ResultType>(lhs: ErrorOr<ResultType>, rhs: _OptionalNilComparisonType) -> Bool {
  return Optional(errorOrResult: lhs) == rhs
}

func ==<ResultType>(lhs: _OptionalNilComparisonType, rhs: ErrorOr<ResultType>) -> Bool {
  return lhs == Optional(errorOrResult: rhs)
}

func !=<ResultType>(lhs: ErrorOr<ResultType>, rhs: _OptionalNilComparisonType) -> Bool {
  return Optional(errorOrResult: lhs) != rhs
}

func !=<ResultType>(lhs: _OptionalNilComparisonType, rhs: ErrorOr<ResultType>) -> Bool {
  return lhs != Optional(errorOrResult: rhs)
}
