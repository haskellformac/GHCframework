//
//  Range.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 26/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


func fromNSRange(range: NSRange) -> Range<Int> {
  return range.location ..< NSMaxRange(range)
}

func toNSRange(range: Range<Int>) -> NSRange {
  return NSRange(location: range.startIndex, length: range.endIndex - range.startIndex)
}
