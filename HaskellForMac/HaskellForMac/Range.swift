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

func extendRange(range: Range<Int>, offsets: (prec: UInt, succ: UInt)) -> Range<Int> {
  return (range.startIndex - Int(offsets.prec)) ..< (range.endIndex + Int(offsets.succ))
}

func extendRange(range: Range<Line>, offsets: (prec: UInt, succ: UInt)) -> Range<Line> {
  if range.startIndex < offsets.prec {
    return 0 ..< (range.endIndex + offsets.succ)
  } else {
    return (range.startIndex - offsets.prec) ..< (range.endIndex + offsets.succ)
  }
}

func clampRange(range: Range<Int>, boundaries: Range<Int>) -> Range<Int> {
  if range.startIndex > boundaries.endIndex || range.endIndex < boundaries.startIndex { return 0..<0 }

  return max(range.startIndex, boundaries.startIndex) ..< min(range.endIndex, boundaries.endIndex)
}

func clampRange(range: Range<Line>, boundaries: Range<Line>) -> Range<Line> {
  if range.startIndex > boundaries.endIndex || range.endIndex < boundaries.startIndex {
    return boundaries.startIndex..<boundaries.startIndex
  }
  return max(range.startIndex, boundaries.startIndex) ..< min(range.endIndex, boundaries.endIndex)
}