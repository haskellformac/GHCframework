//
//  UtilitiesTests.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 23/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Tests for code in Utilities/

import Cocoa
import XCTest
import Haskell

class UtilitiesTests: XCTestCase {

  override func setUp() {
    super.setUp()
    // Put setup code here. This method is called before the invocation of each test method in the class.
  }
    
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }


  // MARK: -
  // MARK: Tests for 'nextName()'

  func test_nextName_empty() {
    XCTAssert(nextName("Name", []) == "Name", "Pass")
  }

  func test_nextName_duplicate() {
    XCTAssert(nextName("Name", ["Name"]) == "Name2", "Pass")
  }

  func test_nextName_triple() {
    XCTAssert(nextName("Name", ["Name", "Name2"]) == "Name3", "Pass")
  }
  
  func test_nextName_backToBase() {
    XCTAssert(nextName("Name", ["Name2", "Name3"]) == "Name", "Pass")
  }
  
  func test_nextName_backToBase2() {
    XCTAssert(nextName("Name", ["Name1", "Name3"]) == "Name", "Pass")
  }


  // MARK: -
  // MARK: Tests for 'StringLineMap'

  func test_StringLineMap_empty() {
    let emptyString                            = ""
    var mapForEmptyString: StringLineMap<Bool> = StringLineMap(string: emptyString)

    XCTAssertEqual(mapForEmptyString.lastLine, 1)
    XCTAssertEqual(mapForEmptyString.startOfLine(0)!, emptyString.endIndex)
    XCTAssertEqual(mapForEmptyString.startOfLine(1)!, emptyString.startIndex)
    XCTAssertEqual(mapForEmptyString.endOfLine(0), emptyString.endIndex)
    XCTAssertEqual(mapForEmptyString.endOfLine(1), emptyString.endIndex)
    XCTAssertEqual(mapForEmptyString.infoOfLine(1), [])
    XCTAssertTrue(mapForEmptyString.startOfLine(2) == nil)

    mapForEmptyString.addLineInfo([(1, true), (2, false)])
    XCTAssertEqual(mapForEmptyString.infoOfLine(1), [true])

    mapForEmptyString.replaceLineInfo([(1, [false, true])])
    XCTAssertEqual(mapForEmptyString.infoOfLine(1), [false, true])
    XCTAssertEqual(mapForEmptyString.infoOfLine(2), [])
  }

  func test_StringLineMap_3lines() {
    let string                            = "\nHello World!\nI'm here"
    var mapForString: StringLineMap<Bool> = StringLineMap(string: string)

    XCTAssertEqual(mapForString.lastLine, 3)
    XCTAssertEqual(mapForString.startOfLine(0)!, string.endIndex)
    XCTAssertEqual(mapForString.startOfLine(1)!, string.startIndex)
    XCTAssertEqual(mapForString.endOfLine(1), advance(string.startIndex, 1))
    XCTAssertEqual(mapForString.startOfLine(2)!, advance(string.startIndex, 1))
    XCTAssertEqual(mapForString.endOfLine(2), advance(string.startIndex, 14))
    XCTAssertEqual(mapForString.startOfLine(3)!, advance(string.startIndex, 14))
    XCTAssertEqual(mapForString.endOfLine(4), advance(string.startIndex, 22))
  }

//    func testPerformanceExample() {
//        // This is an example of a performance test case.
//        self.measureBlock() {
//            // Put the code you want to measure the time of here.
//        }
//    }

}
