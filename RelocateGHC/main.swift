//
//  main.swift
//  RelocateGHC
//
//  Created by Manuel M T Chakravarty on 12/09/2014.
//  Copyright (c) [2014..2017] Manuel M T Chakravarty. All rights reserved.
//
//  Rewrite the launch scripts and package database configuration files to the current location of the 'GHC.framework'.
//
//  This script finds the 'GHC.framework' in one of two ways. Firstly, it checks whether the script is embedded in 
//  the framework, and secondly, it checks whether the framework is in the current working directory.
//
//  In the former case, the script may be invoked in sandboxed mode by passing the '--sandboxed' flag. In sandboxed mode,
//  we don't relocate executables and create a copy of the rewritten package database in the app container (as writing
//  to the app bundle would be a sandbox violation.) In sandboxed mode, the flag is being followed by two further 
//  arguments: (1) the 'NSApplicationSupportDirectory' and (2) the 'CFBundleVersion'.

import Foundation


// MARK: Helpers

infix operator ?!

// Abort if nil.
//
func ?!<A>(v: A?, msg: String) -> A {
  if let v = v {
    return v
  } else {
    NSLog(msg)
    exit(1)
  }
}

// Determine the location at which the libraries for the given package conf file are.
//
func libraryLocation(_ conf: String) -> String? {
  if let startOfLoc  = conf.range(of: "library-dirs: ")?.upperBound,
         let endOfLoc    = conf.range(of: "/usr/lib", options: [], range: startOfLoc..<conf.endIndex)?.lowerBound
  {
    return String(conf[startOfLoc..<endOfLoc])
  } else { return nil }
}

// Replace all occurences of one string in a file by a new string, possibly writing the result to a new file
//
func replaceInFile(_ source: String, string oldString: String, withString newString: String, writingTo target: String? = nil)
{
  do {
    try String(contentsOfFile: source).replacingOccurrences(of: oldString, with: newString)
      .write(toFile: target ?? source, atomically: false, encoding: String.Encoding.utf8)
                                        // ^^^^if we'd do it atomically, we'd kill the executable permissions on the scripts
  } catch _ {
  }
}


// MARK: Determine target location

let bundleLocation = (NSURL(fileURLWithPath: Bundle.main.bundlePath) as NSURL)
                     .appendingPathComponent("..")!.resolvingSymlinksInPath(),
    location       = bundleLocation.path.hasSuffix("GHC.framework/Versions/A") // NB: need to use 'A', not 'Current', as this is canonical
                     ? bundleLocation
                     : URL(fileURLWithPath: FileManager.default.currentDirectoryPath).appendingPathComponent("GHC.framework/Versions/A")

let relativeBin   = "usr/bin",
    relativeLib   = "usr/lib/ghc",
    relativeShare = "usr/share",
    executables   = ["ghc", "ghci", "ghc-pkg", "hpc", "hsc2hs", "runghc", "haddock"],
    ghcLibPath    = location.appendingPathComponent(relativeLib),
    embeddedShare = location.appendingPathComponent(relativeShare),
    packageDBPath = ghcLibPath.appendingPathComponent("package.conf.d")

let rtsConfPath   = packageDBPath.appendingPathComponent("rts.conf"),
    rtsConfData   = (try? String(contentsOfFile: rtsConfPath.path))
                    ?! ("fatal error: could not load RTS package configuration \(rtsConfPath)")

let oldLocation   = libraryLocation(rtsConfData)
                    ?! ("fatal error: could not extract library path from 'library-dirs' field")

if location == URL(fileURLWithPath: oldLocation) {
  exit(0)  // location is already up to date
}
print("Relocating from \(oldLocation) to \(location.path)")


// MARK: Process arguments

let defaultFileManager = FileManager.default

var appContainerGHCRoot:           URL? = nil
var appContainerLib:               URL? { get {
  return appContainerGHCRoot?.appendingPathComponent("lib")
} }
var appContainerGHCLib:            URL? { get {
  return appContainerLib?.appendingPathComponent("ghc")
} }
var appContainerBundleVersionPath: URL? { get {
  return appContainerLib?.appendingPathComponent("Version")
} }
var appContainerBin:               URL? { get {
  return appContainerGHCRoot?.appendingPathComponent("bin")
} }
var appContainerShare:             URL? { get {
  return appContainerGHCRoot?.appendingPathComponent("share")
} }
var appContainerPackageDBPath:     URL? { get {
  return appContainerGHCLib?.appendingPathComponent("package.conf.d")
} }
var bundleVersion: String? = nil

if CommandLine.argc == 4 && CommandLine.arguments[1] == "--sandboxed" {

  appContainerGHCRoot = URL(fileURLWithPath: CommandLine.arguments[2])
  bundleVersion       = CommandLine.arguments[3]

    // To consider the package database up to date, the following three conditions must hold:
    // (1) The current 'bundleVersion' and that stored in the app container need to be the same.
    // (2) The location of the library embedded in HfM and the location encoded in the package specs in the package
    //     database located in the app container need to match.
    // (3) The package cache embedded in HfM needs to be older than that in the app container.
  let appContainerRtsConfPath    = appContainerPackageDBPath!.appendingPathComponent("rts.conf")
    // FIXME: the following needs to have error handling cleaned up
  let appContainerBundleVersion = (try? String(contentsOfFile: appContainerBundleVersionPath!.path)) ?? "UNKNOWN",
      str:           String?    = try? String(contentsOfFile: appContainerRtsConfPath.path)
  if let appContainerRtsConfData = str,
//  if let appContainerRtsConfData = try String(contentsOfFile: appContainerRtsConfPath.path!),
         let appContainerLocation    = libraryLocation(appContainerRtsConfData)    // library location used in app container DB
  {
    if URL(fileURLWithPath: appContainerLocation) == location && appContainerBundleVersion == bundleVersion {

      let embeddedCachePath      = packageDBPath.appendingPathComponent("package.cache"),
          embeddedCacheAttrs     = try? defaultFileManager.attributesOfItem(atPath: embeddedCachePath.path),
          appContainerCachePath  = appContainerPackageDBPath!.appendingPathComponent("package.cache"),
          appContainerCacheAttrs = try? defaultFileManager.attributesOfItem(atPath: appContainerCachePath.path)
      if let embeddedCacheDate      = (embeddedCacheAttrs as NSDictionary?)?.fileModificationDate(),
              let appContainerCacheDate = (appContainerCacheAttrs as NSDictionary?)?.fileModificationDate(), appContainerCacheDate.compare(embeddedCacheDate) == .orderedDescending
      {
        exit(0)  // location and timestamp of app-container package-db are up to date
      }
    }

      // Remove the out of date GHC root directory including package database as a whole. (The new one may have
      // different package sets, versions, or package IDs, in which case, droppings of the old may remain if we
      // just write over the old root directory.)
    print("Removing old GHC root directory including package DB in app container")
    if let lib = appContainerLib, let bin = appContainerBin, let share = appContainerShare {

      do {
        if defaultFileManager.fileExists(atPath: lib.path)   { try defaultFileManager.removeItem(atPath: lib.path) }
        if defaultFileManager.fileExists(atPath: bin.path)   { try defaultFileManager.removeItem(atPath: bin.path) }
        if defaultFileManager.fileExists(atPath: share.path) { try defaultFileManager.removeItem(atPath: share.path) }
      }
      catch let error {
        NSLog("failed to remove existing file at GHC root directory location in app container: \(error)")
        exit(1)
      }

    } else {

      NSLog("failed to remove existing file at GHC root directory location in app container: no directory path")
      exit(1)

    }
  }
  print("Updating GHC root directory and package DB in app container")

}

var sandboxed: Bool { get { return appContainerPackageDBPath != nil } }


// MARK: Start rewriting

  // Rewrite the location in all executables if not in sandboxed mode.
if !sandboxed {
  for executable in
    (executables.map{ location.appendingPathComponent(relativeBin).appendingPathComponent($0) })
  {
    replaceInFile(executable.path, string: oldLocation, withString: location.path)
  }
}

  // In sandboxed mode, create the GHC root directory, package DB & ghc/bin path in the app container if they don't exit yet.
if let packageDBPath = appContainerPackageDBPath {

  var isDirectory: ObjCBool = false
  let package_conf_d_exists = defaultFileManager.fileExists(atPath: packageDBPath.path, isDirectory: &isDirectory)
  if !package_conf_d_exists {                         // doesn't exist => create directory

    do {
      try defaultFileManager.createDirectory(atPath: packageDBPath.path,
                                                   withIntermediateDirectories: true,
                                                   attributes: nil)
      try defaultFileManager.createDirectory(atPath: appContainerGHCLib!.appendingPathComponent("bin").path,
                                                   withIntermediateDirectories: true,
                                                   attributes: nil)
      defaultFileManager.createFile(atPath: appContainerBundleVersionPath!.path,
                                          contents: bundleVersion!.data(using: String.Encoding.utf8),
                                          attributes: nil)
    } catch let error as NSError {
      NSLog("failed to create package DB (or bin directory) in app container: %@", error)
      exit(1)
    }

  } else if !isDirectory.boolValue {                            // plain file exists => remove & create directory

    do {
      try defaultFileManager.removeItem(atPath: packageDBPath.path)
    } catch let error as NSError {
      NSLog("failed to remove existing file at package DB location in app container: %@", error)
      exit(1)
    }
    do {
      // FIXME: de-duplicate by using a local function
      try defaultFileManager.createDirectory(atPath: packageDBPath.path,
                                                   withIntermediateDirectories: true,
                                                   attributes: nil)
      try defaultFileManager.createDirectory(atPath: appContainerGHCLib!.appendingPathComponent("bin").path,
                                                   withIntermediateDirectories: true,
                                                   attributes: nil)
      defaultFileManager.createFile(atPath: appContainerBundleVersionPath!.path,
                                          contents: bundleVersion!.data(using: String.Encoding.utf8),
                                          attributes: nil)
    } catch let error as NSError {
      NSLog("failed to create package DB in app container: %@", error)
      exit(1)
    }

  }
}

  // Remove all old .conf files
if let packageDBPath = appContainerPackageDBPath {

  let ghcPackageDBDir   = (try? defaultFileManager.contentsOfDirectory(atPath: packageDBPath.path))
                          ?! ("fatal error: could not read package DB directory at \(packageDBPath)"),
      packageConfFiles  = ghcPackageDBDir.filter{ $0.hasSuffix(".conf") }

  for confFile in packageConfFiles { try defaultFileManager.removeItem(at: packageDBPath.appendingPathComponent(confFile)) }
}

let ghcLibDir   = (try? defaultFileManager.contentsOfDirectory(atPath: ghcLibPath.path))
                  ?! ("fatal error: could not read directory containing the GHC library at \(ghcLibPath)"),
    ghcLibFiles = ghcLibDir.filter{ $0 != "package.conf.d" }

  // In sandboxed mode, create symbolic links in the app container too all files in the embedded GHC root directory
  // with the exception of the package DB.
if appContainerPackageDBPath != nil {
  do {
    for ghcRootFile in ghcLibFiles {

      let source = ghcLibPath.appendingPathComponent(ghcRootFile),
          target = appContainerGHCLib!.appendingPathComponent(ghcRootFile)
      if defaultFileManager.fileExists(atPath: target.path) {
        try defaultFileManager.removeItem(at: target)
      }
      try defaultFileManager.createSymbolicLink(at: target, withDestinationURL: source)

    }
  } catch let error as NSError {
    NSLog("failed to create symbolic links to populate GHCLIB directory in app container: %@", error)
    exit(1)
  }
}

let shareFiles = (try? defaultFileManager.contentsOfDirectory(atPath: embeddedShare.path))
                 ?! ("fatal error: could not read share/ directory at \(embeddedShare)")

  // In sandboxed mode, create the share/ directory if it doesn't exist yet and populate it with symbolic links to the
  // files in the share/ directory embedded in the app.
if let sharePath = appContainerShare {

  let shareDirExists = defaultFileManager.fileExists(atPath: sharePath.path, isDirectory: nil)
  if !shareDirExists {                            // doesn't exist => create directory

    do {
      try defaultFileManager.createDirectory(atPath: sharePath.path,
                                                   withIntermediateDirectories: true,
                                                   attributes: nil)
    } catch let error as NSError {
      NSLog("failed to create share directory in app container: %@", error)
      exit(1)
    }
    for shareFile in shareFiles {

      let source = embeddedShare.appendingPathComponent(shareFile),
          target = appContainerShare!.appendingPathComponent(shareFile)
      try defaultFileManager.createSymbolicLink(at: target, withDestinationURL: source)

    }
  }
}

let packagesDir = (try? defaultFileManager.contentsOfDirectory(atPath: packageDBPath.path))
                  ?!  ("fatal error: could not read directory containing GHC package database at \(packageDBPath)"),
    packages    = packagesDir.filter{ $0.hasSuffix("conf") }

  // Rewrite the location in all package 'conf' files, writing to the app container in sandboxed mode.
for package in packages {

  let source = packageDBPath.appendingPathComponent(package),
      target = appContainerPackageDBPath?.appendingPathComponent(package)
  replaceInFile(source.path, string: oldLocation, withString: location.path, writingTo: target?.path)

}

  // In sandboxed mode, add an rpath linker option pointing to the app container GHC library path to the RTS conf.
if let builtin_rts     = (packages.filter{ $0.hasSuffix("rts.conf") }).first,
       let builtin_rtsPath = appContainerPackageDBPath?.appendingPathComponent(builtin_rts).path
{
  do {

    let contents    = try String(contentsOfFile: builtin_rtsPath),
        rpathOption = "            \"-Wl,-rpath,\(ghcLibPath.path)\""
    try (contents + rpathOption).write(toFile: builtin_rtsPath, atomically: false, encoding: String.Encoding.utf8)

  } catch _ {
    NSLog("Failed to add RPATH to rts.conf")
    exit(1)
  }
}

  // Refresh the package db cache.
let ghcPkgTask: Process
if let packageDBPath = appContainerPackageDBPath {

  let ghcPkgPath = location.appendingPathComponent("Executables/ghc-pkg")
  ghcPkgTask = Process.launchedProcess(launchPath: ghcPkgPath.path, arguments: ["--global-package-db", packageDBPath.path, "recache"])

} else {

    // In the non-sandboxed case, we use the script on purpose as an extra sanity check (that the scripts have been
    // rewritten properly).
  let ghcPkgPath = location.appendingPathComponent(relativeBin).appendingPathComponent("ghc-pkg")
  ghcPkgTask = Process.launchedProcess(launchPath: ghcPkgPath.path, arguments: ["recache"])

}
ghcPkgTask.waitUntilExit()
if ghcPkgTask.terminationStatus != 0 {
  NSLog("regenerating the binary package cache failed")
  exit(1)
}
