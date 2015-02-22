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
  "  other-modules: OtherSource\n"

let oneOtherSourceCabalFileWrapper =
  NSFileWrapper(regularFileWithContents: oneOtherSourceCabalFile.dataUsingEncoding(NSUTF8StringEncoding)!)
let otherSourceHSWrapper           = NSFileWrapper(regularFileWithContents: NSData())
let oneOtherSourceHSProjWrapper    = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal":     oneOtherSourceCabalFileWrapper
                                                                              , "Test.hs":        testHSWrapper
                                                                              , "OtherSource.hs": otherSourceHSWrapper])

let sourceDirCabalFile =
  "name: Test\n" +
  "version: 1.0\n" +
  "cabal-version: 1.6\n" +
  "build-type: Simple\n" +
  "Executable FileTest\n" +
  "  main-is: Test.hs\n" +
  "  other-modules: OtherSource\n" +
  "  hs-source-dirs: src\n"

let sourceDirCabalFileWrapper   =
  NSFileWrapper(regularFileWithContents: sourceDirCabalFile.dataUsingEncoding(NSUTF8StringEncoding)!)
let srcWrapper                  = NSFileWrapper(directoryWithFileWrappers: [ "Test.hs":        testHSWrapper
                                                                           , "OtherSource.hs": otherSourceHSWrapper])
let sourceDirHSProjWrapper      = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal":     sourceDirCabalFileWrapper
                                                                           , "src":            srcWrapper])

let otherDirCabalFile =
  "name: Test\n" +
  "version: 1.0\n" +
  "cabal-version: 1.6\n" +
  "build-type: Simple\n" +
  "Executable FileTest\n" +
  "  main-is: Test.hs\n" +
  "  other-modules: Other.OtherSource\n" +
  "  hs-source-dirs: src\n"

let otherDirCabalFileWrapper   =
  NSFileWrapper(regularFileWithContents: otherDirCabalFile.dataUsingEncoding(NSUTF8StringEncoding)!)
let otherWrapper               = NSFileWrapper(directoryWithFileWrappers: [ "OtherSource.hs": otherSourceHSWrapper ])
let srcOtherWrapper            = NSFileWrapper(directoryWithFileWrappers: [ "Test.hs": testHSWrapper
                                                                          , "Other":   otherWrapper])
let otherDirHSProjWrapper      = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal":     otherDirCabalFileWrapper
                                                                          , "src":            srcOtherWrapper])


class ViewModelTests: XCTestCase {

  override func setUp() {
    super.setUp()

    minimalHSProjWrapper.preferredFilename = "Test.hsproj"
    oneOtherSourceHSProjWrapper.preferredFilename = "Test.hsproj"
    sourceDirHSProjWrapper.preferredFilename = "Test.hsproj"
  }
  
  override func tearDown() {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    super.tearDown()
  }

  func test_fileWrapperForFileGroup() {
    let dstWrapper    = NSFileWrapper(directoryWithFileWrappers: [ "Test.hs":        testHSWrapper
                                                                 , "OtherSource.hs": otherSourceHSWrapper])
    let srcDstWrapper = NSFileWrapper(directoryWithFileWrappers: [ "dst": dstWrapper ])
    let rootWrapper   = NSFileWrapper(directoryWithFileWrappers: [ "Test.cabal":     sourceDirCabalFileWrapper
                                                                 , "src":            srcDstWrapper])

    XCTAssert(fileWrapperForFileGroup(sourceDirHSProjWrapper, "*.h") === sourceDirHSProjWrapper,
      "Picked wrong wrapper for '*.h'")
    XCTAssert(fileWrapperForFileGroup(sourceDirHSProjWrapper, "src") === srcWrapper,
      "Picked wrong wrapper for 'src'")
    XCTAssert(fileWrapperForFileGroup(sourceDirHSProjWrapper, "src/*.h") === srcWrapper,
      "Picked wrong wrapper for 'src/*.h'")

    XCTAssert(fileWrapperForFileGroup(rootWrapper, "src") === srcDstWrapper,
      "Picked wrong wrapper for 'src' (in 'src/dst')")
    XCTAssert(fileWrapperForFileGroup(rootWrapper, "src/dst") === dstWrapper,
      "Picked wrong wrapper for 'src/dst'")
    XCTAssert(fileWrapperForFileGroup(rootWrapper, "src/dst/*") === dstWrapper,
      "Picked wrong wrapper for 'src/dst/*'")

    XCTAssert(fileWrapperForFileGroup(sourceDirHSProjWrapper, "dst") === sourceDirHSProjWrapper,
      "Picked wrong wrapper for nonexistent path")
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

    let otherSourceItem: ProjectItem = executableItem.children[0]   // children are sorted by identifiers
    XCTAssertEqual(otherSourceItem.children.count, 0)
    XCTAssertEqual(otherSourceItem.identifier, "OtherSource.hs")
    XCTAssertEqual(otherSourceItem.fileWrapper!.preferredFilename, "OtherSource.hs")

    let mainFileItem: ProjectItem = executableItem.children[1]
    XCTAssertEqual(mainFileItem.children.count, 0)
    XCTAssertEqual(mainFileItem.identifier, "Test.hs")
    XCTAssertEqual(mainFileItem.fileWrapper!.preferredFilename, "Test.hs")
}

  func testSourceDirStructure() {
    let viewModel = HFMProjectViewModel(projectFileWrapper: sourceDirHSProjWrapper,
                                        cabalFileWrapper: sourceDirCabalFileWrapper,
                                        documentURL: NSURL(fileURLWithPath: "/tmp/Test.hsproj"))  // this is fake/not used
    let groupItems: ProjectItemGroups = viewModel.groupItems

    XCTAssertEqual(viewModel.groupItems.packageGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.executableGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.extraSourceGroupItem.children.count, 0)
    XCTAssertEqual(viewModel.groupItems.dataGroupItem.children.count, 0)

    let executableItem: ProjectItem = viewModel.groupItems.executableGroupItem.children[0]
    XCTAssert(executableItem.isExecutable, "Executable Group?")
    XCTAssertEqual(executableItem.children.count, 1)
    XCTAssertEqual(executableItem.identifier, "FileTest")
    XCTAssertEqual(executableItem.fileWrapper!.preferredFilename, "Test.hsproj")

    let srcDirItem: ProjectItem = executableItem.children[0]
    XCTAssertEqual(srcDirItem.children.count, 2)
    XCTAssertEqual(srcDirItem.identifier, "src")
    XCTAssertEqual(srcDirItem.fileWrapper!.preferredFilename, "src")

    let otherSourceItem: ProjectItem = srcDirItem.children[0]   // children are sorted by identifiers
    XCTAssertEqual(otherSourceItem.children.count, 0)
    XCTAssertEqual(otherSourceItem.identifier, "OtherSource.hs")
    XCTAssertEqual(otherSourceItem.fileWrapper!.preferredFilename, "OtherSource.hs")

    let mainFileItem: ProjectItem = srcDirItem.children[1]
    XCTAssertEqual(mainFileItem.children.count, 0)
    XCTAssertEqual(mainFileItem.identifier, "Test.hs")
    XCTAssertEqual(mainFileItem.fileWrapper!.preferredFilename, "Test.hs")
  }

  func testOtherDirStructure() {
    let viewModel = HFMProjectViewModel(projectFileWrapper: otherDirHSProjWrapper,
      cabalFileWrapper: otherDirCabalFileWrapper,
      documentURL: NSURL(fileURLWithPath: "/tmp/Test.hsproj"))  // this is fake/not used
    let groupItems: ProjectItemGroups = viewModel.groupItems

    XCTAssertEqual(viewModel.groupItems.packageGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.executableGroupItem.children.count, 1)
    XCTAssertEqual(viewModel.groupItems.extraSourceGroupItem.children.count, 0)
    XCTAssertEqual(viewModel.groupItems.dataGroupItem.children.count, 0)

    let executableItem: ProjectItem = viewModel.groupItems.executableGroupItem.children[0]

    let srcDirItem: ProjectItem = executableItem.children[0]
    XCTAssertEqual(srcDirItem.children.count, 2)
    XCTAssertEqual(srcDirItem.identifier, "src")
    XCTAssertEqual(srcDirItem.fileWrapper!.preferredFilename, "src")

    let otherItem: ProjectItem = srcDirItem.children[0]            // children are sorted by identifiers
    XCTAssertEqual(otherItem.children.count, 1)
    XCTAssertEqual(otherItem.identifier, "Other")
    XCTAssertEqual(otherItem.fileWrapper!.preferredFilename, "Other")

    let mainFileItem: ProjectItem = srcDirItem.children[1]
    XCTAssertEqual(mainFileItem.children.count, 0)
    XCTAssertEqual(mainFileItem.identifier, "Test.hs")
    XCTAssertEqual(mainFileItem.fileWrapper!.preferredFilename, "Test.hs")

    let otherSourceItem: ProjectItem = otherItem.children[0]   // children are sorted by identifiers
    XCTAssertEqual(otherSourceItem.children.count, 0)
    XCTAssertEqual(otherSourceItem.identifier, "OtherSource.hs")
    XCTAssertEqual(otherSourceItem.fileWrapper!.preferredFilename, "OtherSource.hs")
  }
}