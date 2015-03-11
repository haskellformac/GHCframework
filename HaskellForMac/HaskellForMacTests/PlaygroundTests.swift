//
//  PlaygroundTests.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 6/03/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import XCTest
import Haskell


let simplePlayground1 = "test"
let simplePlayground2 = "test\n"
let simplePlayground3 = "test\n "
let simplePlayground4 = "test\n \n"
let simplePlayground5 = "\ntest\n \n"

let multipleCommands =
  "let x = 1\n" +
  "\n" +
  "    y = 2\n" +
  "\n" +
  "x + y\n" +
  "\n" +
  "\n" +
  "  \n" +
  "data D = A\n" +
  "       | B\n" +
  "       | C"

func commandsForPlaygroundText(text: String) -> (PlaygroundCommands, CodeStorageDelegate) {
  let codeStorage      = NSTextStorage(attributedString: NSAttributedString(string: text))
  let delegate         = CodeStorageDelegate(textStorage: codeStorage)
  codeStorage.delegate = delegate
  return (PlaygroundCommands(codeStorage: codeStorage), delegate) // Keep the delegate ref!! NSTextStorage's ref is only weak!
}

class PlaygroundTests: XCTestCase {

  override func setUp() {
    super.setUp()
    // Put setup code here. This method is called before the invocation of each test method in the class.
  }
  
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  func testEmptyPlayground() {
    let (commands, delegate) = commandsForPlaygroundText("")

    XCTAssertEqual(commands.count, 0)
    XCTAssert(commands.queryCommand(0) == nil, "out of bounds command")
  }

  func testSimplePlayground() {
    let (commands1, delegate1) = commandsForPlaygroundText(simplePlayground1)
    let (commands2, delegate2) = commandsForPlaygroundText(simplePlayground2)
    let (commands3, delegate3) = commandsForPlaygroundText(simplePlayground3)
    let (commands4, delegate4) = commandsForPlaygroundText(simplePlayground4)
    let (commands5, delegate5) = commandsForPlaygroundText(simplePlayground5)

    XCTAssertEqual(commands1.count, 1)
    XCTAssertEqual(commands1.queryCommand(0)!.lines, 1...1)
    XCTAssertEqual(commands2.count, 1)
    XCTAssertEqual(commands2.queryCommand(0)!.lines, 1...1)
    XCTAssertEqual(commands3.count, 1)
    XCTAssertEqual(commands3.queryCommand(0)!.lines, 1...2)
    XCTAssertEqual(commands4.count, 1)
    XCTAssertEqual(commands4.queryCommand(0)!.lines, 1...2)
    XCTAssertEqual(commands5.count, 1)
    XCTAssertEqual(commands5.queryCommand(0)!.lines, 2...3)
  }

  func testMultipleCommands() {
    let (commands, delegate) = commandsForPlaygroundText(multipleCommands)

    XCTAssertEqual(commands.count, 3)
    XCTAssertEqual(commands.queryCommand(0)!.lines, 1...4)
    XCTAssertEqual(commands.queryCommand(1)!.lines, 5...8)
    XCTAssertEqual(commands.queryCommand(2)!.lines, 9...11)
  }
}
