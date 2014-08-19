//
//  StringExtensions.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


// MARK: -
// MARK: Extension to 'NSString' (i.e., the underlying text storage)

// FIXME: this version can go once we have rewritten TextGutter to use Switf strings
extension NSString {
  func lineNumberAtLocation(loc: Int) -> UInt {
    switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
    case 0: return 1
    case let start: return lineNumberAtLocation(start - 1) + 1
    }
  }
}

extension String {
  func lineNumberAtLocation(loc: String.Index) -> UInt {
    let startOfLine = self.lineRangeForRange(loc...loc).startIndex
    if startOfLine == self.startIndex {
      return 1
    } else {
      return lineNumberAtLocation(advance(startOfLine, -1)) + 1
    }
  }
}
