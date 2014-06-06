//
//  CabalTest.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 5/06/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import XCTest

class CabalTest: XCTestCase {

  var projectViewModel: HFMProjectViewModel = HFMProjectViewModel()

  override func setUp() {
    super.setUp()
    projectViewModel = HFMProjectViewModel()
  }
    
  override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  func testSetVersion() {
    let version = "1.0.0"
    projectViewModel.version = version
    XCTAssert(projectViewModel.version == version, "Pass")
  }

  func testMeasureSetVersion() {
    self.measureBlock() {
      let projectViewModel = HFMProjectViewModel()
      let version = "1.0.0"
      projectViewModel.version = version
    }
  }

}
