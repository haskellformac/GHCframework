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
public struct StringLineMap<LineInfo> {
  private typealias MapElement = (index: String.Index, utf16Index: Int, info: [LineInfo])
  private var map: [MapElement]

  public var lastLine: Line {
    get {
      return map.count <= 1 ? 0 : UInt(map.count - 1)
    }
  }

  public init(string: String) {
    map = [(string.endIndex, string.utf16Count, [])]

      // Iterate through the lines.
    var idx: String.Index = string.startIndex
    do {
      let newIndex: MapElement = (index: idx, string[string.startIndex..<idx].utf16Count, info: [])
      map.append(newIndex)
      idx = string.lineRangeForRange(idx..<idx).endIndex
    } while idx < string.endIndex

      // Make sure to add an extra line if the last character is a newline (hence, additional empty last line).
    let newlines = NSCharacterSet.newlineCharacterSet()
    if !string.isEmpty && newlines.characterIsMember(string.utf16[string.utf16.endIndex - 1]) {
      let newIndex: MapElement = (index: string.endIndex, string.utf16Count, info: [])
      map.append(newIndex)
    }
  }

  public func startOfLine(line: Line) -> String.Index? {
    return line <= lastLine ? map[Int(line)].index : nil
  }

  private func utf16StartOfLine(line: Line) -> Int {
    return line <= lastLine ? map[Int(line)].utf16Index : 0
  }

  public func infoOfLine(line: Line) -> [LineInfo] {
    return line <= lastLine ? map[Int(line)].info : []
  }

  public func endOfLine(line: Line) -> String.Index {
    return line >= lastLine ? map[0].index : map[Int(line + 1)].index
  }

  /// Determine the line range covered by the given character range according to the line map.
  ///
  /// This returns an empty range iff the character range is empty or out of bounds.
  ///
  public func lineRange(charRange: Range<Int>) -> Range<Line> {
    if charRange.isEmpty || charRange.startIndex < 0 || charRange.endIndex > utf16StartOfLine(0) {
      return 0..<0
    }

    return line(charRange.startIndex) ... line(charRange.endIndex - 1)
  }

  // Determine the line on which a character is by binary search.
  //
  private func line(charIndex: Int) -> Line {
    var lineRange = 0...lastLine
    while lineRange.endIndex - lineRange.startIndex > 1 {
      let middle = Line(lineRange.startIndex + (lineRange.endIndex - lineRange.startIndex) / 2)
      let middleCharIndex = utf16StartOfLine(middle)
      if charIndex < middleCharIndex {
        lineRange = lineRange.startIndex..<middle
      } else {
        lineRange = middle..<lineRange.endIndex
      }
    }
    return (lineRange.startIndex == lineRange.endIndex) ? 0  : lineRange.startIndex
  }

  /// Extend the line information for all the given lines.
  ///
  /// Line info for lines not already in the map is dropped.
  ///
  public mutating func addLineInfo(lineInfo: [(Line, LineInfo)]) {
    for (line, newInfo) in lineInfo {
      if let index = startOfLine(line) {
        var info = infoOfLine(line)
        info.append(newInfo)
        map[Int(line)] = (index, utf16StartOfLine(line), info)
      }
    }
  }

  /// Substitute the line information for the given lines.
  ///
  /// Line info for lines not already in the map is dropped.
  ///
  public mutating func replaceLineInfo(lineInfo: [(Line, [LineInfo])]) {
    for (line, newInfo) in lineInfo {
      if let index = startOfLine(line) {
        map[Int(line)] = (index, utf16StartOfLine(line), newInfo)
      }
    }
  }
}


// MARK: -
// MARK: Extensions to 'NSString' (i.e., the underlying text storage)

// FIXME: this version can go once we have rewritten TextGutter to use Swift strings; arggghhh...and we need it in
//        TextEditorController in `textStorageDidProcessEditing()`
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
