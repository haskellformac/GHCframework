//
//  StringExtensions.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation

// MARK: -
// MARK: Line maps

/// A mapping from line numbers to (a) the starting index of a line in the associated string and (b) an array of generic
/// line information (e.g., an array of tokens on that line).
///
/// If the array is `lineMap`, then `lineMap[0]` is initialised, but unused; `lineMap[1]` is the info for the 1st line,
/// `lineMap[2]` for the second, and so on.
///
/// The `indexOfLine(0)` is the `string.endIndex` of the associated string.
///
struct StringLineMap<LineInfo> {
  private var map: [(index: String.Index, info: [LineInfo])]

  var lastLine: Line {
    get {
      return map.count <= 0 ? 0 : UInt(map.count - 1)
    }
  }

  init(string: String) {
    map = [(string.endIndex, [])]

    var idx: String.Index = string.startIndex
    while idx < string.endIndex {
      let newIndex: (index: String.Index, info: [LineInfo]) = (index: idx, info: [])
      map.append(newIndex)
      idx = string.lineRangeForRange(idx...idx).endIndex
    }
  }

  func startOfLine(line: Line) -> String.Index? {
    return line < lastLine ? map[Int(line)].index : nil
  }

  func infoOfLine(line: Line) -> [LineInfo] {
    return line < lastLine ? map[Int(line)].info : []
  }

  func endOfLine(line: Line) -> String.Index {
    return line >= lastLine ? map[0].index : map[Int(line + 1)].index
  }

  /// Extend the line information for all the given lines.
  ///
  /// Line info for lines not already in the map is dropped.
  ///
  mutating func addLineInfo(lineInfo: [(Line, LineInfo)]) {
    for (line, newInfo) in lineInfo {
      if let index = startOfLine(line) {
        var info = infoOfLine(line)
        info.append(newInfo)
        map[Int(line)] = (index, info)
      }
    }
  }

  /// Substitute the line information for the given lines.
  ///
  /// Line info for lines not already in the map is dropped.
  ///
  mutating func replaceLineInfo(lineInfo: [(Line, [LineInfo])]) {
    for (line, newInfo) in lineInfo {
      if let index = startOfLine(line) {
        map[Int(line)] = (index, newInfo)
      }
    }
  }
}


// MARK: -
// MARK: Extensions to 'NSString' (i.e., the underlying text storage)

// FIXME: this version can go once we have rewritten TextGutter to use Swift strings
extension NSString {
  func lineNumberAtLocation(loc: Int) -> UInt {
    switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
    case 0: return 1
    case let start: return lineNumberAtLocation(start - 1) + 1
    }
  }
}

// MARK: -
// MARK: Extensions to Swift Strings

extension String {
  func lineNumberAtLocation(loc: String.Index) -> UInt {
    let startOfLine = self.lineRangeForRange(loc...loc).startIndex
    if startOfLine == self.startIndex {
      return 1
    } else {
      return lineNumberAtLocation(advance(startOfLine, -1)) + 1
    }
  }

  func replicate(n: Int) -> String {
    return (n <= 0) ? "" : self + self.replicate(n - 1)
  }
}
