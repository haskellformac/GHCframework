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
//  A command must start with a line that has no white space in the first column and extends up to the next line
//  that has no white space in the first column. A command may include trailing empty (only whitespace) lines and may
//  have empty lines interspersed with code lines.


import Foundation


public struct PlaygroundCommands {

  public struct Command {
    let lines: Range<Line>        // The lines in the code view of the playground making up the command
    let dirty: Bool               // Whether the command needs to be evaluated

    public init(lines: Range<Line>, dirty: Bool) {self.lines = lines; self.dirty = dirty} // Swift 1.1 doesn't accept it as public otherwise
  }

  /// All commands currently in the playground
  ///
  private var commands: [Command] = []

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

public func ==(lhs: PlaygroundCommands.Command, rhs: PlaygroundCommands.Command) -> Bool {
  return lhs.lines == rhs.lines && lhs.dirty == rhs.dirty
}

extension PlaygroundCommands.Command: Equatable { }

extension PlaygroundCommands.Command: Printable {

  public var description: String { get { return "(lines = \(self.lines); dirty = \(self.dirty))" } }
}


// MARK: -
// MARK: Retrieving information from the underlying code view.

extension PlaygroundCommands {

  /// Retrieve the nth command.
  ///
  public func queryCommand(n: Int) -> Command? {
    if n < commands.endIndex { return commands[n] } else { return nil }
  }

}

extension PlaygroundCommands {

  /// Extract the commands in the associated code storage from scratch.
  ///
  public mutating func scanCodeStorage () {
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
      func scanCommandAtLine(startLine: Line) -> Command? {
        let string          = codeStorage.string.utf16
        let length          = codeStorage.string.utf16Count
        let whitespaceChars = NSCharacterSet.whitespaceAndNewlineCharacterSet()

        let startIndex = codeStorageDelegate.charRangeOfLine(startLine).startIndex
        if startIndex >= length || whitespaceChars.characterIsMember(string[startIndex]) { return nil }
        
        func lineCompletesCommand(line: Line) -> Bool {
          let charRange = codeStorageDelegate.charRangeOfLine(line)
          let endIndex  = charRange.endIndex
          return endIndex >= length || !whitespaceChars.characterIsMember(string[endIndex])
        }
        
        var endLine: Line = startLine
        while !lineCompletesCommand(endLine) { endLine++ }
        return Command(lines: startLine...endLine, dirty: true)
      }

      commands       = []
      var line: Line = 1
      while line <= codeStorageDelegate.lineMap.lastLine {

        if let command = scanCommandAtLine(line) {

          commands.append(command)
          line = command.lines.endIndex     // guaranteed to make progress due to postcondition of `scanCommandAtLine(_:)`

        } else { line++ }                   // this line had no command => skip
      }
    }
  }
}
