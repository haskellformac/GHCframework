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
func nextName(baseName: String, names: [String]) -> String {
  func suffixToInt(suffix: String) -> Int? {
    return suffix.isEmpty ? 1 : suffix.toInt()
  }

  let n         = baseName.endIndex
  let suffixes  = names.filter{ $0.hasPrefix(baseName) }.map{ (s: String) in suffixToInt(s[n ..< s.endIndex]) }
  let newSuffix = suffixes.reduce(1){ (current, usedSuffix: Int?) in
    switch usedSuffix {
    case .None: return current
    case .Some(let v): return v >= current ? v + 1 : current
    } }
  return (newSuffix == 1) ? baseName : baseName + newSuffix.description
}

@objc class Swift {
  class func nextNameOf(baseName: String, usedNames: [String]) -> String {
    return nextName(baseName, usedNames)
  }
}