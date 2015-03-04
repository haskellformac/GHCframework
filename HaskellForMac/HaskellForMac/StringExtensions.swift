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
/// The `startOfLine(0)` is the `endIndex` of the associated string.
///
public struct StringLineMap<LineInfo> {

  // NB: We store indices as `Int` as we need to work with positions in GHC tokens. Moreover, we would need to retain
  //     the underlying string (which is changing as a document gets edited) to do conversions on the fly.
  //
  private typealias MapElement = (index: Int, info: [LineInfo])

  private var map: [MapElement]

  /// The last line that has got any characters.
  ///
  public var lastLine: Line {
    get {
      return map.count <= 1 ? 0 : UInt(map.count - 1)
    }
  }

  public init(string: String) {
// Swift 1.2:    map = [(count(string.utf16), [])]
    map = [(string.utf16Count, [])]

      // Iterate through the lines.
    var idx = string.startIndex
    do {
      let newIndex: MapElement = (index: string.stringIndexToInt(idx), info: [])
      map.append(newIndex)
      idx = string.lineRangeForRange(idx..<idx).endIndex
    } while idx < string.endIndex

      // Make sure to add an extra line if the last character is a newline (hence, additional empty last line).
    let newlines = NSCharacterSet.newlineCharacterSet()
    if !string.isEmpty && newlines.characterIsMember(string.utf16[string.utf16.endIndex - 1]) {
// Swift 1.2:      let newIndex: MapElement = (index: count(string.utf16), info: [])
      let newIndex: MapElement = (index: string.utf16Count, info: [])
      map.append(newIndex)
    }
  }

  public func startOfLine(line: Line) -> Int? {
    return line <= lastLine ? map[Int(line)].index : nil
  }

  public mutating func setStartOfLine(line: Line, startIndex: Int) {
    if line <= lastLine {
      let newIndex: MapElement = (index: startIndex, info: infoOfLine(line))
      map[Int(line)] = newIndex
    }
  }

  public func infoOfLine(line: Line) -> [LineInfo] {
    return line <= lastLine ? map[Int(line)].info : []
  }

  public func endOfLine(line: Line) -> Int {
    return line >= lastLine ? map[0].index : map[Int(line + 1)].index
  }

  /// Determine the line range covered by the given character range according to the line map.
  ///
  /// This returns an empty range iff the character range is out of bounds, but returns a one line range if the
  /// character range is within bounds, but empty (namely the line the empty range is located on).
  ///
  public func lineRange(charRange: Range<Int>) -> Range<Line> {
    if charRange.startIndex < 0 || charRange.endIndex > startOfLine(0) {
      return 1..<1
    }

    if charRange.isEmpty {
      let oneLine = line(charRange.startIndex)
      return oneLine...oneLine
    } else {
      return line(charRange.startIndex) ... line(charRange.endIndex - 1)
    }
  }

  /// Determine the line on which a character is according to the line map by binary search.
  ///
  public func line(charIndex: Int) -> Line {
    var lineRange = 0...lastLine
    while lineRange.endIndex - lineRange.startIndex > 1 {
      let middle = Line(lineRange.startIndex + (lineRange.endIndex - lineRange.startIndex) / 2)
      let middleCharIndex = startOfLine(middle)
      if charIndex < middleCharIndex {
        lineRange = lineRange.startIndex..<middle
      } else {
        lineRange = middle..<lineRange.endIndex
      }
    }
    return (lineRange.startIndex == lineRange.endIndex) ? 1 : lineRange.startIndex
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
        map[Int(line)] = (index, info)
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
        map[Int(line)] = (index, newInfo)
      }
    }
  }
}


// MARK: -
// MARK: Extensions to 'NSString'

extension NSString {

  /// Compute the line number of a character location, using a line map if available.
  ///
  func lineNumber<LineInfo>(lineMap: StringLineMap<LineInfo>?, atLocation loc: Int) -> UInt {
    if let lineMap = lineMap { return lineMap.line(loc) }
    else {
      switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
      case 0:         return 1
      case let start: return lineNumber(lineMap, atLocation: start - 1) + 1
      }
    }
  }

  /// Determines the indentation (initial span of whitespace characters) of the given line.
  ///
  func indentOf<LineInfo>(lineMap: StringLineMap<LineInfo>, line: Line) -> Int {
    if let startIndex = lineMap.startOfLine(line) {

      let whitespace = NSCharacterSet.whitespaceCharacterSet()
      var i          = startIndex
      while (i < self.length && whitespace.characterIsMember(self.characterAtIndex(i))) { i++ }
      return i - startIndex

    } else { return 0 }
  }
}


// MARK: -
// MARK: Extensions to Swift Strings

extension String {

  /// Convert a string index into a plain `Int` index.
  ///
  /// In the precense of `Foundation`, this should be O(1) as `UTF16Index` is a `RandomAccessIndexType`.
  ///
  func stringIndexToInt(idx: String.Index) -> Int {
// Swift 1.2:    return distance(self.utf16.startIndex, idx.samePositionIn(self.utf16))
    return self[self.startIndex..<idx].utf16Count
  }

  /// Compute the line number of a character location, using a line map if available.
  ///
  func lineNumber<LineInfo>(lineMap: StringLineMap<LineInfo>?, atLocation loc: String.Index) -> UInt {
    return lineNumber(lineMap, atLocation: self.stringIndexToInt(loc))
  }
}
