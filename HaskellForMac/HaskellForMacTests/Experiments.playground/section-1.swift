// Playground - noun: a place where people can play

import Cocoa

let myText = "This is a long piece of text, which is spread\n\n\nover multiple lines,\nand we need to figure out how many lines\nthere are."

myText.rangeOfString("which")
let range: Range<String.Index> = advance(myText.startIndex, 47)...advance(myText.startIndex, 49)

myText
myText.lineRangeForRange(range)
myText.lineRangeForRange(myText.startIndex...myText.startIndex)
myText.startIndex

extension String {
  func lineNumberAtLocation(loc: String.Index) -> UInt {
    switch self.lineRangeForRange(loc...loc).startIndex {
    case self.startIndex: return 1
    case let start: return lineNumberAtLocation(advance(start, -1)) + 1
    }
  }
}

myText.lineNumberAtLocation(advance(myText.startIndex, 66))
myText[myText.startIndex...advance(myText.startIndex, 66)]

let myNSText = myText as NSString
let r = myNSText.rangeOfString("which")

myNSText.lineRangeForRange(NSRange(location: 0, length: 0))
myNSText.characterAtIndex(0)

("a\n\n" as NSString).lineRangeForRange(NSRange(location: 2, length: 1))

extension NSString {
  func lineNumberAtLocation(loc: Int) -> Int {
    switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
    case 0: return 1
    case let start: return lineNumberAtLocation(start - 1) + 1
    }
  }
}

myNSText.lineNumberAtLocation(67)

//

typealias Line   = UInt
typealias Column = UInt

/// Source code locations
///
struct SrcLoc {
  let file:   String
  let line:   Line
  let column: Column
}

/// Source code spans
///
/// The 'endColumn' is the column *after* the last character included in the span.
///
struct SrcSpan {
  let start:     SrcLoc
  let lines:     UInt
  let endColumn: Column
}

/// Severity of an issue
///
enum Severity {case Warning, Error}

/// A single source code issue.
///
struct Issue {
  let span:     SrcSpan
  let severity: Severity
  let message:  String
}

/// A set of issues flagged by the compiler for a single source file can be indexed by line number. All issues
/// appearing on a single line are sorted by their starting column.
///
typealias Issues = [Column: [Issue]]

/// All issues flagged by the compiler for a single source file including the file name.
///
struct IssuesForFile {
  let file:   String
  let issues: Issues
}

var issue: Issue = Issue(span: SrcSpan(start: SrcLoc(file: "f", line: 1, column: 1), lines: 1, endColumn: 1), severity: .Error, message: "")


var issues: Issues = [UInt(1): [Issue(span: SrcSpan(start: SrcLoc(file: "f", line: 1, column: 1), lines: 1, endColumn: 1), severity: .Error, message: "")]]

let i = issues[1]
issues[1]?[0].severity
