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
  
//    func testPerformanceExample() {
//        // This is an example of a performance test case.
//        self.measureBlock() {
//            // Put the code you want to measure the time of here.
//        }
//    }

}
