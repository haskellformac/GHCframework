//
//  HighlightingTests.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 25/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import XCTest
import Haskell
import GHCKit


// Makes the testing code more convenient.
extension String
{
  subscript(range: Range<Int>) -> String
    {
      return (self as NSString).substringWithRange(NSRange(location: range.startIndex,
                                                             length: range.endIndex - range.startIndex))
  }
}

let session: HaskellSession = HaskellSession(diagnosticsHandler: { _, _, _, _, _, _, _ in },
                                             interactiveWorkingDirectory: "/tmp")

func tokeniser(file: String) -> HighlightingTokeniser {
  return { (line, column, text) in
    map(session.tokeniseHaskell(text, file: file, line: line, column: column)){ token in
                                                                                  HighlightingToken(ghcToken: token) }
  }
}

let simpleProgram =
  "map :: (a -> b) -> [a] -> [b]\n" +
  "map f [] = []\n" +
  "map f (x:xs) = f x : f xs"


class HighlightingTests: XCTestCase {

  override func setUp() {
    super.setUp()
    // Put setup code here. This method is called before the invocation of each test method in the class.
  }
    
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  // MARK: -
  // MARK: Tests for 'LineTokenMap'

  func test_LineTokenMap_1line() {
    let program                = "answer = 42"
    let answerToken            = HighlightingToken(ghcToken: Token(kind: .Varid, filename: "test",
                                                                   line: 1, column: 1, lines: 1, endColumn: 7))
    let equalToken             = HighlightingToken(ghcToken: Token(kind: .Equal, filename: "test",
                                                   line: 1, column: 8, lines: 1, endColumn: 9))
    let n42Token               = HighlightingToken(ghcToken: Token(kind: .Integer, filename: "test",
                                                   line: 1, column: 10, lines: 1, endColumn: 12))
    var tokenMap: LineTokenMap = lineTokenMap(program){ _program in
      [answerToken, equalToken, n42Token]
    }

    // Initialisation tests
    XCTAssertEqual(tokenMap.lastLine, Line(1))
    let line1Info = tokenMap.infoOfLine(1)
    XCTAssertEqual(line1Info.count, 3)
    XCTAssertEqual(line1Info[0], answerToken)
    XCTAssertEqual(line1Info[1], equalToken)
    XCTAssertEqual(line1Info[2], n42Token)

    // Tokenisation tests
    let generateTokens = tokensAtLine(tokenMap)
    let allTokens      = [].join((1...1).map(generateTokens))
    XCTAssertTrue(map(allTokens){ t in let (a, _b) = t; return a } == [answerToken, equalToken, n42Token])
    func snd<S, T>(pair: (S, snd: T)) -> T { return pair.snd }
    let t0 = program[snd(allTokens[0])]     // need to let bind to avoid crashing the Swift compiler
    XCTAssertEqual(t0, "answer")
    let t1 = program[snd(allTokens[1])]
    XCTAssertEqual(t1, "=")
    let t2 = program[snd(allTokens[2])]
    XCTAssertEqual(t2, "42")
  }

  func test_LineTokenMap_2line() {
    let program                = "answer =\n  42  -- Yo!"
    let answerToken            = HighlightingToken(ghcToken: Token(kind: .Varid, filename: "test",
                                                   line: 1, column: 1, lines: 1, endColumn: 7))
    let equalToken             = HighlightingToken(ghcToken: Token(kind: .Equal, filename: "test",
                                                   line: 1, column: 8, lines: 1, endColumn: 9))
    let n42Token               = HighlightingToken(ghcToken: Token(kind: .Integer, filename: "test",
                                                   line: 2, column: 3, lines: 1, endColumn: 5))
    let commentToken           = HighlightingToken(ghcToken: Token(kind: .Integer, filename: "test",
                                                   line: 2, column: 7, lines: 1, endColumn: 13))
    var tokenMap: LineTokenMap = lineTokenMap(program){ _program in
      [answerToken, equalToken, n42Token, commentToken]
    }

    XCTAssertEqual(tokenMap.lastLine, Line(2))
    let line1Info = tokenMap.infoOfLine(1)
    XCTAssertEqual(line1Info.count, 2)
    XCTAssertEqual(line1Info[0], answerToken)
    XCTAssertEqual(line1Info[1], equalToken)
    let line2Info = tokenMap.infoOfLine(2)
    XCTAssertEqual(line2Info.count, 2)
    XCTAssertEqual(line2Info[0], n42Token)
    XCTAssertEqual(line2Info[1], commentToken)

    // Tokenisation tests
    let generateTokens = tokensAtLine(tokenMap)
    let allTokens      = [].join((1...2).map(generateTokens))
    XCTAssertTrue(map(allTokens){ t in let (a, _b) = t; return a } == [answerToken, equalToken, n42Token, commentToken])
    func snd<S, T>(pair: (S, snd: T)) -> T { return pair.snd }
    let t0 = program[snd(allTokens[0])]     // need to let bind to avoid crashing the Swift compiler
    XCTAssertEqual(t0, "answer")
    let t1 = program[snd(allTokens[1])]
    XCTAssertEqual(t1, "=")
    let t2 = program[snd(allTokens[2])]
    XCTAssertEqual(t2, "42")
    let t3 = program[snd(allTokens[3])]
    XCTAssertEqual(t3, "-- Yo!")
  }

  func test_LineTokenMap_3line() {
    let program                = "answer =\n  42  {- Yo!\ncool -}"
    let answerToken            = HighlightingToken(ghcToken: Token(kind: .Varid, filename: "test",
                                                   line: 1, column: 1, lines: 1, endColumn: 7))
    let equalToken             = HighlightingToken(ghcToken: Token(kind: .Equal, filename: "test",
                                                   line: 1, column: 8, lines: 1, endColumn: 9))
    let n42Token               = HighlightingToken(ghcToken: Token(kind: .Integer, filename: "test",
                                                   line: 2, column: 3, lines: 1, endColumn: 5))
    let commentToken           = HighlightingToken(ghcToken: Token(kind: .Integer, filename: "test",
                                                   line: 2, column: 7, lines: 2, endColumn: 8))
    var tokenMap: LineTokenMap = lineTokenMap(program){ _program in
      [answerToken, equalToken, n42Token, commentToken]
    }

    XCTAssertEqual(tokenMap.lastLine, Line(3))
    let line1Info = tokenMap.infoOfLine(1)
    XCTAssertEqual(line1Info.count, 2)
    XCTAssertEqual(line1Info[0], answerToken)
    XCTAssertEqual(line1Info[1], equalToken)
    let line2Info = tokenMap.infoOfLine(2)
    XCTAssertEqual(line2Info.count, 2)
    XCTAssertEqual(line2Info[0], n42Token)
    XCTAssertEqual(line2Info[1], commentToken)
    let line3Info = tokenMap.infoOfLine(3)
    XCTAssertEqual(line3Info.count, 1)
    XCTAssertEqual(line3Info[0], commentToken)

    // Tokenisation tests
    let generateTokens = tokensAtLine(tokenMap)
    XCTAssertEqual([].join((1...1).map(generateTokens)).count, 2)
    XCTAssertEqual([].join((2...2).map(generateTokens)).count, 2)
    XCTAssertEqual([].join((3...3).map(generateTokens)).count, 1)
    let allTokens      = [].join((1...3).map(generateTokens))
    XCTAssertTrue(map(allTokens){ t in let (a, _b) = t; return a } == [answerToken, equalToken, n42Token, commentToken,                                               commentToken])
    func snd<S, T>(pair: (S, snd: T)) -> T { return pair.snd }
    let t0 = program[snd(allTokens[0])]     // need to let bind to avoid crashing the Swift compiler
    XCTAssertEqual(t0, "answer")
    let t1 = program[snd(allTokens[1])]
    XCTAssertEqual(t1, "=")
    let t2 = program[snd(allTokens[2])]
    XCTAssertEqual(t2, "42")
    let t3 = program[snd(allTokens[3])]
    XCTAssertEqual(t3, "{- Yo!\n")
    let t4 = program[snd(allTokens[4])]
    XCTAssertEqual(t4, "cool -}")
  }

  func test_LineTokenMap_simpleProgram_tokenise() {
    var tokenMap: LineTokenMap = lineTokenMap(simpleProgram, tokeniser("test"))

    XCTAssertEqual(tokenMap.lastLine, Line(3))
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 3).count, 13)
  }

  func test_LineTokenMap_simpleProgram_withNewline() {
    var tokenMap: LineTokenMap = lineTokenMap(simpleProgram + "\n", tokeniser("test"))

    XCTAssertEqual(tokenMap.lastLine, Line(4))
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 3).count, 13)
    XCTAssertEqual(tokensAtLine(tokenMap)(line: 4).count, 0)
  }
  
  func test_LineTokenMap_simpleProgram_addNewline() {
    let tokenMap: LineTokenMap = lineTokenMap(simpleProgram, tokeniser("test"))

    let editedProgram  = simpleProgram + "\n"
// Swift 1.1:    let editedRange    = simpleProgram.utf16Count..<editedProgram.utf16Count
    let editedRange    = count(simpleProgram.utf16)..<count(editedProgram.utf16)
    let changeInLength = 1

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(4))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 3).count, 13)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 4).count, 0)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! + 1)
  }

  func test_LineTokenMap_simpleProgram_addTwoNewline() {
    let tokenMap: LineTokenMap = lineTokenMap(simpleProgram, tokeniser("test"))

    let editedProgram  = simpleProgram + "\n\n"
// Swift 1.1:    let editedRange    = simpleProgram.utf16Count..<editedProgram.utf16Count
    let editedRange    = count(simpleProgram.utf16) ..< count(editedProgram.utf16)
    let changeInLength = 2

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(5))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 3).count, 13)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 4).count, 0)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 5).count, 0)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! + 2)
  }

  func test_LineTokenMap_simpleProgram_removeNewline() {
    let tokenMap: LineTokenMap = lineTokenMap(simpleProgram + "\n\n", tokeniser("test"))

    let editedProgram  = simpleProgram + "\n"
    // Swift 1.1:    let editedRange    = simpleProgram.utf16Count..<editedProgram.utf16Count
    let editedRange    = count(editedProgram.utf16) ..< count(editedProgram.utf16)
    let changeInLength = -1

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(4))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 3).count, 13)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 4).count, 0)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! - 1)
  }

  func test_LineTokenMap_simpleProgram_removeNewlineFront() {
    let tokenMap: LineTokenMap = lineTokenMap("\n" + simpleProgram + "\n\n", tokeniser("test"))

    let editedProgram  = simpleProgram + "\n\n"
    let editedRange    = 0 ..< 1
    let changeInLength = -1

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(5))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 7)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 3).count, 13)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 4).count, 0)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 5).count, 0)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! - 1)
  }

  func test_LineTokenMap_simpleProgram_removeNewlineMiddle() {
    let tokenMap: LineTokenMap = lineTokenMap(simpleProgram, tokeniser("test"))

    let editedProgram  = "map :: (a -> b) -> [a] -> [b]\n" +
                         "map f [] = []" +
                         "map f (x:xs) = f x : f xs"
    let prefixIdx      = count(("map :: (a -> b) -> [a] -> [b]\n" + "map f [] = []").utf16)
    let editedRange    = prefixIdx ..< prefixIdx + 1
    let changeInLength = -1

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(2))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 20)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! - 1)
  }

  func test_LineTokenMap_simpleProgram_addNewlineMiddle() {
    let tokenMap: LineTokenMap = lineTokenMap(simpleProgram, tokeniser("test"))

    let editedProgram  = "map :: (a -> b) -> [a] -> [b]\n" +
                         "map f []\n = []\n" +
                         "map f (x:xs) = f x : f xs"
    let prefixIdx      = count(("map :: (a -> b) -> [a] -> [b]\n" + "map f []").utf16)
    let editedRange    = prefixIdx ..< prefixIdx + 1
    let changeInLength = 1

    let editedTokenMap = tokenMapProcessEdit(tokenMap, editedProgram, editedRange, changeInLength, tokeniser("test"))

    XCTAssertEqual(editedTokenMap.lastLine, Line(4))
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 1).count, 15)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 2).count, 4)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 3).count, 3)
    XCTAssertEqual(tokensAtLine(editedTokenMap)(line: 4).count, 13)
    XCTAssertEqual(editedTokenMap.startOfLine(0)!, tokenMap.startOfLine(0)! + 1)
  }
}


class ThemeTests: XCTestCase {

  override func setUp() {
    super.setUp()
    // Put setup code here. This method is called before the invocation of each test method in the class.
  }

  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  func test_themeAttributeSerialisation() {

    func themeAttributeRoundTrip(theme: ThemeAttributes) -> ThemeAttributes {
      return dictionaryToThemeAttributes(themeAttributesToDictionary(theme))
    }

    let solarLightKeyColour =
      ThemeAttributes(foreground: NSColor(SRGBRed:  41/255, green:  66/255, blue: 119/255, alpha: 1), underline: false)
    XCTAssertEqual(solarLightKeyColour, themeAttributeRoundTrip(solarLightKeyColour))
  }

  func test_themeSerialisation() {

    func themeRoundTrip(theme: Theme) -> Theme {
      return dictionaryToTheme(themeToDictionary(theme))
    }

    XCTAssertEqual(defaultThemes[0], themeRoundTrip(defaultThemes[0]))
    XCTAssertEqual(defaultThemes[1], themeRoundTrip(defaultThemes[1]))
    XCTAssertEqual(defaultThemes[2], themeRoundTrip(defaultThemes[2]))
    XCTAssertEqual(defaultThemes[3], themeRoundTrip(defaultThemes[3]))
    XCTAssertEqual(defaultThemes, defaultThemes.map(themeRoundTrip))
  }
}


