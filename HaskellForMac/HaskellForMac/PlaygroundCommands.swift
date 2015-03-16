//
//  PlaygroundCommands.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 5/03/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  Keeps track of the commands in a playground including their location in the code view and whether they need to be
//  evaluated.
//
//  A command must start with a line that has no white space in the first column and extends over all immediately
//  following lines whose first character is a whitespace. A command may include trailing empty (only whitespace) lines
//  and may have empty lines interspersed with code lines. This emulates Haskell's off-side rule.
//
//  Commands are evaluated asynchronously. To ensure the consistency of the playground commands data structures and 
//  access to it, all *reading and writing* to playground commands must be performed on the main thread.

import Foundation


public struct PlaygroundCommands {

  public enum Status: Printable {
    case LastEdited(NSDate)
    case LastEvaluated(NSDate)
  }

  public enum EvaluationStatus {
    case NothingToDo                // No command needs to be evaluated.
    case Evaluate(Int)              // The command at the given index needs to be evaluated next.
    case Evaluating(Int)            // The command at the given index is currently being evaluated.
  }

  public struct Command {
    public let index: Int
    public let text:  String
    public let lines: Range<Line>
  }

  /// All commands currently in the playground
  ///
  private var commands: [Range<Line>] = []

  /// Index of the next command that needs to be evaluated.
  ///
  private var nextCommand: EvaluationStatus = .NothingToDo

  /// The number of available commands.
  ///
  public var count: Int { get { return commands.count } }

  /// The delegate of the code storage whose commands this struct tracks.
  ///
  private var codeStorage: NSTextStorage

  public init(codeStorage: NSTextStorage) {
    self.codeStorage = codeStorage
    scanCodeStorage()
  }
}

extension PlaygroundCommands.Status: Printable {
  public var description: String { get {
    switch self {
    case .LastEdited(let timestamp):    return ".LastEdited: \(timestamp)"
    case .LastEvaluated(let timestamp): return ".LastEvaluated: \(timestamp)"
    } } }
}

extension PlaygroundCommands.Command: Printable {
  public var description: String { get { return "(index = \(self.index); text = \(self.text))" } }
}


// MARK: -
// MARK: Retrieving information from the underlying code view.

extension PlaygroundCommands {

  /// Next command that needs to be evaluated if any.
  ///
  /// NB: This function is *not* idempotent. A pending command can only be obtained once.
  ///
  public mutating func nextPendingCommand() -> Command? {
    switch nextCommand {
    case .Evaluate(let idx): nextCommand = .Evaluating(idx); return queryCommand(idx)
    default:                 return nil
    }
  }

  /// Reset all command evaluation and start from scratch (e.g., because the module context changed).
  ///
  public mutating func setAllCommandsPending() {
    if count > 0 { nextCommand = .Evaluate(0) }
  }

  /// Marks the given command as being completed *iff* it is the currently pending command and still has the same text.
  ///
  /// The return value indicates whether marking completed successfully.
  ///
  public mutating func markAsCompleted(command: Command) -> Bool {
    switch nextCommand {
    case .Evaluating(let pendingIndex):
      if pendingIndex == command.index && queryCommand(pendingIndex)?.text == command.text {

        nextCommand = (pendingIndex + 1 < count) ? .Evaluate(pendingIndex + 1) : .NothingToDo
        return true

      } else { return false }
    default: return false
    }
  }

  /// Retrieve the nth command.
  ///
  public func queryCommand(n: Int) -> Command? {
    if n < commands.endIndex {
      if let delegate = codeStorage.delegate as? CodeStorageDelegate {

        let lines     = commands[n]
        let charRange = delegate.charRangeOfLines(lines)
        return Command(index: n,
                       text: (codeStorage.string as NSString).substringWithRange(toNSRange(charRange)),
                       lines: lines)

      }
    }
    return nil
  }
}

private func mycount<T : _CollectionType>(x: T) -> T.Index.Distance { return count(x) }  /// really?!?...

extension PlaygroundCommands {

  /// Extract the commands in the associated code storage from scratch.
  ///
  public mutating func scanCodeStorage() {
    if let codeStorageDelegate = codeStorage.delegate as? CodeStorageDelegate {

      /// A command must start with a line that has no white space in the first column and extends up to the next line
      /// that has no white space in the first column.
      ///
      /// A command may include trailing empty (only whitespace) lines and may have empty lines interspersed with code
      /// lines.
      ///
      /// Precondition:  The line must be part of the code view storage according to the line map.
      /// Postcondition: The line range of the returned command encompasses at least one line — i.e., it is not empty.
      ///
      func scanCommandAtLine(startLine: Line) -> Range<Line>? {
        let string          = codeStorage.string.utf16
// Swift 1.1:        let length          = codeStorage.string.utf16Count
        let length          = mycount(string)
        let whitespaceChars = NSCharacterSet.whitespaceAndNewlineCharacterSet()

        let startIndex = codeStorageDelegate.charRangeOfLine(startLine).startIndex
// Swift 1.1:        if startIndex >= length || whitespaceChars.characterIsMember(string[startIndex]) { return nil }
        if startIndex >= length || whitespaceChars.characterIsMember(string[String.UTF16Index(startIndex)]) { return nil }
        
        func lineCompletesCommand(line: Line) -> Bool {
          let charRange = codeStorageDelegate.charRangeOfLine(line)
          let endIndex  = charRange.endIndex
          return endIndex >= length || !whitespaceChars.characterIsMember(string[String.UTF16Index(endIndex)])
        }
        
        var endLine: Line = startLine
        while !lineCompletesCommand(endLine) { endLine++ }
        return startLine...endLine
      }

      commands       = []
      var line: Line = 1
      while line <= codeStorageDelegate.lineMap.lastLine {

        if let lines = scanCommandAtLine(line) {

          commands.append(lines)
          line = lines.endIndex             // guaranteed to make progress due to postcondition of `scanCommandAtLine(_:)`

        } else { line++ }                   // this line had no command => skip
      }
    }
    setAllCommandsPending()
  }
}
