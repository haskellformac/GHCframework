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
    XCTAssertEqual(tokenMap.lastLine, 1)
    let line1Info = tokenMap.infoOfLine(1)
    XCTAssertEqual(line1Info.count, 3)
    XCTAssertEqual(line1Info[0], answerToken)
    XCTAssertEqual(line1Info[1], equalToken)
    XCTAssertEqual(line1Info[2], n42Token)

    // Tokenisation tests
    let generateTokens = tokensWithSpan(tokenMap)
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

    XCTAssertEqual(tokenMap.lastLine, 2)
    let line1Info = tokenMap.infoOfLine(1)
    XCTAssertEqual(line1Info.count, 2)
    XCTAssertEqual(line1Info[0], answerToken)
    XCTAssertEqual(line1Info[1], equalToken)
    let line2Info = tokenMap.infoOfLine(2)
    XCTAssertEqual(line2Info.count, 2)
    XCTAssertEqual(line2Info[0], n42Token)
    XCTAssertEqual(line2Info[1], commentToken)

    // Tokenisation tests
    let generateTokens = tokensWithSpan(tokenMap)
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

    XCTAssertEqual(tokenMap.lastLine, 3)
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
    let generateTokens = tokensWithSpan(tokenMap)
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

//    func testPerformanceExample() {
//        // This is an example of a performance test case.
//        self.measureBlock() {
//            // Put the code you want to measure the time of here.
//        }
//    }

}
