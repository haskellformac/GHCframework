//
//  ViewModelTests.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 20/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Haskell
import Cocoa
import XCTest


let minimalCabalFile =
  "name: Test\n" +
  "version: 1.0\n" +
  "cabal-version: 1.6\n" +
  "build-type: Simple\n" +
  "Executable FileTest\n" +
  "  main-is: Test.hs\n"

let minimalCabalFileWrapper = NSFileWrapper(regularFileWithContents: minimalCabalFile.dataUsingEncoding(NSUTF8StringEncoding)!)
let testHSWrapper           = NSFileWrapper(regularFileWithContents: NSData())
let minimalHSProjWrapper    = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal": minimalCabalFileWrapper
                                                                       , "Test.hs":    testHSWrapper])

let oneOtherSourceCabalFile =
  "name: Test\n" +
  "version: 1.0\n" +
  "cabal-version: 1.6\n" +
  "build-type: Simple\n" +
  "Executable FileTest\n" +
  "  main-is: Test.hs\n" +
  "  other-modules: OtherSource"

let oneOtherSourceCabalFileWrapper =
  NSFileWrapper(regularFileWithContents: oneOtherSourceCabalFile.dataUsingEncoding(NSUTF8StringEncoding)!)
let otherSourceHSWrapper           = NSFileWrapper(regularFileWithContents: NSData())
let oneOtherSourceHSProjWrapper    = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal":     oneOtherSourceCabalFileWrapper
                                                                              , "Test.hs":        testHSWrapper
                                                                              , "OtherSource.hs": otherSourceHSWrapper])


class ViewModelTests: XCTestCase {

  override func setUp() {
    super.setUp()

    minimalHSProjWrapper.preferredFilename = "Test.hsproj"
    oneOtherSourceHSProjWrapper.preferredFilename = "Test.hsproj"
  }
  
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  func testMinimalStructure() {
    let viewModel = HFMProjectViewModel(projectFileWrapper: minimalHSProjWrapper,
                                        cabalFileWrapper: minimalCabalFileWrapper,
                                        documentURL: NSURL(fileURLWithPath: "/tmp/Test.hsproj"))  // this is fake/not used
    let groupItems: ProjectItemGroups = viewModel.groupItems

    XCTAssertEqual(viewModel.groupItems.count, 4)
    XCTAssertEqual(viewModel.groupItems.packageGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.executableGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.extraSourceGroupItem.children.count, 0)
    XCTAssertEqual(viewModel.groupItems.dataGroupItem.children.count, 0)

    XCTAssert(viewModel.groupItems.packageGroupItem.isPackageGroup, "Package Group?")

    let cabalFileItem: ProjectItem = viewModel.groupItems.packageGroupItem.children[0]
    let executableItem:  ProjectItem = viewModel.groupItems.executableGroupItem.children[0]
    XCTAssert(executableItem.isExecutable, "Executable Group?")
    XCTAssertEqual(cabalFileItem.children.count, 0)
    XCTAssertEqual(cabalFileItem.identifier, "Test-1.0")
    XCTAssertEqual(cabalFileItem.fileWrapper!.preferredFilename, "Test.cabal")
    XCTAssertEqual(executableItem.children.count, 1)
    XCTAssertEqual(executableItem.identifier, "FileTest")
    XCTAssertEqual(executableItem.fileWrapper!.preferredFilename, "Test.hsproj")
    let mainFileItem:  ProjectItem = executableItem.children[0]
    XCTAssertEqual(mainFileItem.children.count, 0)
    XCTAssertEqual(mainFileItem.identifier, "Test.hs")
    XCTAssertEqual(mainFileItem.fileWrapper!.preferredFilename, "Test.hs")
  }

  func testOneOtherSourceStructure() {
    let viewModel = HFMProjectViewModel(projectFileWrapper: oneOtherSourceHSProjWrapper,
                                        cabalFileWrapper: oneOtherSourceCabalFileWrapper,
                                        documentURL: NSURL(fileURLWithPath: "/tmp/Test.hsproj"))  // this is fake/not used
    let groupItems: ProjectItemGroups = viewModel.groupItems

    XCTAssertEqual(viewModel.groupItems.packageGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.executableGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.extraSourceGroupItem.children.count, 0)
    XCTAssertEqual(viewModel.groupItems.dataGroupItem.children.count, 0)

    let cabalFileItem: ProjectItem = viewModel.groupItems.packageGroupItem.children[0]
    let executableItem: ProjectItem = viewModel.groupItems.executableGroupItem.children[0]
    XCTAssert(executableItem.isExecutable, "Executable Group?")
    XCTAssertEqual(cabalFileItem.children.count, 0)
    XCTAssertEqual(cabalFileItem.identifier, "Test-1.0")
    XCTAssertEqual(cabalFileItem.fileWrapper!.preferredFilename, "Test.cabal")
    XCTAssertEqual(executableItem.children.count, 2)
    XCTAssertEqual(executableItem.identifier, "FileTest")
    XCTAssertEqual(executableItem.fileWrapper!.preferredFilename, "Test.hsproj")
    let mainFileItem: ProjectItem = executableItem.children[0]
    XCTAssertEqual(mainFileItem.children.count, 0)
    XCTAssertEqual(mainFileItem.identifier, "Test.hs")
    XCTAssertEqual(mainFileItem.fileWrapper!.preferredFilename, "Test.hs")

    let otherSourceItem: ProjectItem = executableItem.children[1]
    XCTAssertEqual(otherSourceItem.children.count, 0)
    XCTAssertEqual(otherSourceItem.identifier, "OtherSource.hs")
    XCTAssertEqual(otherSourceItem.fileWrapper!.preferredFilename, "OtherSource.hs")
  }

}
