//
//  Diagnostics.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Model for diagnostics.

import Foundation
import GHCKit


// MARK: -
// MARK: Location information

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


// MARK: -
// MARK: Issues

/// Severity of an issue
///
enum Severity {case Warning, Error, Other}

/// Convert a GHCKit severity value into one of ours.
///
func toSeverity(ghcSeverity: GHCSeverity) -> Severity {
  switch ghcSeverity {
  case .Output, .Dump, .Interactive, .Info, .Fatal:
    return .Other
  case .Warning:
    return .Warning
  case .Error:
    return .Error
  }
}

/// A single source code issue.
///
struct Issue {
  let span:     SrcSpan
  let severity: Severity
  let message:  String

  init(severity:  GHCSeverity,
       filename:  String,
       line:      UInt,
       column:    UInt,
       lines:     UInt,
       endColumn: UInt,
       message:   String) {
    switch severity {
    case .Output, .Dump, .Interactive, .Info, .Fatal:
      NSLog("unrecognised GHC diagnostic %@:", message)
    default:
      break
    }
    self.span     = SrcSpan(start: SrcLoc(file: filename, line: line, column: column),
                            lines: lines,
                            endColumn: endColumn)
    self.severity = toSeverity(severity)
    self.message  = message
  }
}

/// Determine the highest severity of all given issues (if any).
///
func maxSeverityOfIssues(issues: [Issue]) -> Severity? {
  return issues.reduce(nil){ acc, issue in
    if let severity = acc {
      return issue.severity
    } else {
      return issue.severity
    }
  }
}


// MARK: -
// MARK: Issue collections for files

/// A set of issues flagged by the compiler for a single source file can be indexed by line number. All issues
/// appearing on a single line are sorted by their starting column.
///
typealias Issues = [Line: [Issue]]

/// All issues flagged by the compiler for a single source file including the file name.
///
struct IssuesForFile {
  let file:   String
  let issues: Issues
}

/// Add an issue to an issue collection for a file if the filenames match.
///
func addIssueForFile(issue: Issue, issuesForFile: IssuesForFile) -> IssuesForFile {

    // Drop issues whose filename doesn't match.
  if issue.span.start.file != issuesForFile.file {
    return issuesForFile
  }

  let line   = issue.span.start.line
  let column = issue.span.start.column
  var issues = issuesForFile.issues
  if var issuesForLine = issues[line] {

      // Insert new issue after all issues with a smaller than or equal starting column.
    var i = 0
    while i < issuesForLine.count && issuesForLine[i].span.start.column <= column { i++ }
    issuesForLine.insert(issue, atIndex: i)

  } else {

      // First issues on this line.
    issues[line] = [issue]

  }
  return IssuesForFile(file: issuesForFile.file, issues: issues)
}

/// Determine the highest severity of all issues of a file (if any).
///
func maxSeverityOfIssuesForFile(issues: IssuesForFile) -> Severity? {
  return maxSeverityOfIssues(issues.issues.values.array.reduce([], combine: {$0 + $1}))
}


// MARK: -
// MARK: Issues notification

/// Notifications sent to views that display issue sets.
///
enum IssueNotification {
  case NoIssues                         // Checking completed and no issues were found
  case IssuesPending                    // Checking in progress (cue to invalidate old issues)
  case Issues(IssuesForFile)            // Complete set of issues for the file
}
