//
//  NameUtilities.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 20/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation

/// Given a base name and an array of already used names, return a name built from the base name and a numeric suffix,
/// which hasn't been used yet.
///
public func nextName(baseName: String, names: [String]) -> String {
  func suffixToInt(suffix: String) -> Int? {
    return suffix.isEmpty ? 1 : suffix.toInt()
  }

  if !contains(names, baseName) { return baseName }

  let n         = baseName.endIndex
  let suffixes  = names.filter{ $0.hasPrefix(baseName) }.map{ (s: String) in suffixToInt(s[n ..< s.endIndex]) }
  let newSuffix = suffixes.reduce(1){ (current, usedSuffix: Int?) in
    switch usedSuffix {
    case .None: return current
    case .Some(let v): return v >= current ? v + 1 : current
    } }
  return (newSuffix == 1) ? baseName : baseName + newSuffix.description
}

extension Swift {
  class func swift_nextName(baseName: String, usedNames: [String]) -> String {
    return nextName(baseName, usedNames)
  }
}

/// Checks whether the given string is a valid Haskell module name. Haskell defines module names to be constructor
/// identifiers (conid) defined as follows:
///
///   conid -> large {small | large | digit | ' }
///
func isValidModuleName(name: String) -> Bool {
  func isInnerConidChar(ch: UnicodeScalar) -> Bool {
    let primeChar = ("'" as NSString).characterAtIndex(0)
    let maxUInt16 = UInt32(UInt16.max)
    if ch.value > maxUInt16 {
      return false
    } else {
      let chuni = UInt16(ch.value)
      return chuni == primeChar || NSCharacterSet.alphanumericCharacterSet().characterIsMember(chuni)
    }
  }

  let first                 = (name as NSString).characterAtIndex(0)
  let firstCharIsUpper      = NSCharacterSet.uppercaseLetterCharacterSet().characterIsMember(first)
  let restIsAlphaNumOrPrime = all(name.unicodeScalars){ isInnerConidChar($0) }
  return !name.isEmpty && firstCharIsUpper && restIsAlphaNumOrPrime
}

extension Swift {
  class func swift_isValidModuleName(name: String) -> Bool {
    return isValidModuleName(name)
  }
}
