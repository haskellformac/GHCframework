//
//  main.swift
//  RelocateGHC
//
//  Created by Manuel M T Chakravarty on 12/09/2014.
//  Copyright (c) [2014..2015] Manuel M T Chakravarty. All rights reserved.
//
//  Rewrite the launch scripts and package database configuration files to the current location of the 'GHC.framework'.
//
//  This script finds the 'GHC.framework' in one of two ways. Firstly, it checks whether the script is embedded in 
//  the framework, and secondly, it checks whether the framework is in the current working directory.
//
//  In the former case, the script may be invoked in sandboxed mode by passing the '--sandboxed' flag. In sandboxed mode,
//  we don't relocate executables and create a copy of the rewritten package database in the app container (as writing
//  to the app bundle would be a sandbox violation.)

import Foundation


// MARK: Helpers

infix operator ?! { }

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
func libraryLocation(conf: String) -> String? {
  if let startOfLoc  = conf.rangeOfString("library-dirs: ")?.endIndex,
         endOfLoc    = conf.rangeOfString("/usr/lib", options: nil, range: startOfLoc..<conf.endIndex)?.startIndex
  {
    return conf[startOfLoc..<endOfLoc]
  } else { return nil }
}

// Replace all occurences of one string in a file by a new string, possibly writing the result to a new file
//
func replaceInFile(source: String, string oldString: String, withString newString: String, writingTo target: String? = nil)
{
  String(contentsOfFile: source)?.stringByReplacingOccurrencesOfString(oldString, withString: newString)
    .writeToFile(target ?? source, atomically: false, encoding: NSUTF8StringEncoding, error: nil)
                                   // ^^^^if we'd do it atomically, we'd kill the executable permissions on the scripts
}


// MARK: Determine target location

let bundleLocation = NSBundle.mainBundle().bundlePath.stringByAppendingPathComponent("..").stringByResolvingSymlinksInPath
let location       = bundleLocation.hasSuffix("GHC.framework/Versions/A")
                     ? bundleLocation
                     : NSFileManager.defaultManager().currentDirectoryPath.stringByAppendingPathComponent("GHC.framework/Versions/A")

let relativeBin = "usr/bin"
let relativeLib = "usr/lib/ghc"
let executables = ["ghc", "ghci", "ghc-pkg", "hpc", "hsc2hs", "runghc"]
let packageDB    = location.stringByAppendingPathComponent(relativeLib).stringByAppendingPathComponent("package.conf.d")

let rtsConfPath = packageDB.stringByAppendingPathComponent("builtin_rts.conf")
let rtsConfData = String(contentsOfFile: rtsConfPath)
                  ?! ("fatal error: could not load RTS package configuration \(rtsConfPath)")

let oldLocation = libraryLocation(rtsConfData)
                  ?! ("fatal error: could not extract library path from 'library-dirs' field")

/*
let ghcPath     = location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent("ghc")
let ghcScript   = String(contentsOfFile: ghcPath)
                  ?! ("RelocateGHC: fatal error: could not load GHC launch script at \(ghcPath)")

let startOfLoc  = ghcScript.rangeOfString("topdir=\"")?.endIndex
                  ?! ("RelocateGHC: fatal error: could not locate 'topdir' definition")

let endOfLoc    = ghcScript.rangeOfString("/usr/lib", options: nil, range: startOfLoc..<ghcScript.endIndex)?.startIndex
                  ?! ("RelocateGHC: fatal error: could not find end of old location")
let oldLocation = ghcScript[startOfLoc..<endOfLoc]
*/

if location == oldLocation {
  exit(0)  // location is already up to date
}
println("Relocating from \(oldLocation) to \(location)")


// MARK: Process arguments

let defaultFileManager = NSFileManager.defaultManager()

let appContainerPackageDBPath: String?
println("arguments = \(Process.arguments)")
if Process.argc == 3 && Process.arguments[1] == "--sandboxed" {

  appContainerPackageDBPath      = Process.arguments[2].stringByAppendingPathComponent("package.conf.d")
  let appContainerRtsConfPath    = appContainerPackageDBPath!.stringByAppendingPathComponent("builtin_rts.conf")
  if let appContainerRtsConfData = String(contentsOfFile: rtsConfPath),
         appContainerLocation    = libraryLocation(appContainerRtsConfData) where appContainerLocation == location
  {
    exit(0)  // location in app-container package-db is up to date
  }
  println("Updating package DB in app container")

} else { appContainerPackageDBPath = nil }

var sandboxed: Bool { get { return appContainerPackageDBPath != nil } }


// MARK: Start rewriting

  // Rewrite the location in all executables if not in sandboxed mode.
if !sandboxed {
  for executable in
    (executables.map{ location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent($0) })
  {
    replaceInFile(executable, string: oldLocation, withString: location)
  }
}

let packagesDir = defaultFileManager.contentsOfDirectoryAtPath(packageDB, error: nil)
                  ?!  ("fatal error: could not read directory containing GHC package database at \(packageDB)")
let packages    = (packagesDir as? [String])!.filter{ $0.hasSuffix("conf") }
                  ?! ("fatal error: could not load GHC package database")

  // In sandboxed mode, create the package DB in the app container if it doesn't exit yet.
if let packageDBPath = appContainerPackageDBPath {

  var error:       NSError?
  var isDirectory: ObjCBool = false
  let package_conf_d_exists = defaultFileManager.fileExistsAtPath(packageDBPath, isDirectory: &isDirectory)
  if !package_conf_d_exists {                         // doesn't exist => create directory

    if !defaultFileManager.createDirectoryAtPath(packageDBPath,
                                                 withIntermediateDirectories: true,
                                                 attributes: nil,
                                                 error: &error)
    {
      NSLog("failed to create package DB in app container: %@", error!)
      exit(1)
    }

  } else if !isDirectory {                            // plain file exists => remove & create directory

    if !defaultFileManager.removeItemAtPath(packageDBPath, error: &error) {
      NSLog("failed to remove existing file at package DB location in app container: %@", error!)
      exit(1)
    }
    if !defaultFileManager.createDirectoryAtPath(packageDBPath,
                                                 withIntermediateDirectories: true,
                                                 attributes: nil,
                                                 error: &error)
    {
      NSLog("failed to create package DB in app container: %@", error!)
      exit(1)
    }

  }
}

  // Rewrite the location in all package 'conf' files, writing to the app container in sandboxed mode.
for package in packages {

  let source = packageDB.stringByAppendingPathComponent(package),
      target = appContainerPackageDBPath?.stringByAppendingPathComponent(package)
  replaceInFile(source, string: oldLocation, withString: location, writingTo: target)

}

  // Refresh the package db cache.
let ghcPkgTask: NSTask
if let packageDBPath = appContainerPackageDBPath {

  let ghcPkgPath = location.stringByAppendingPathComponent("Executables/ghc-pkg")
  ghcPkgTask = NSTask.launchedTaskWithLaunchPath(ghcPkgPath, arguments: ["--global-package-db", packageDBPath, "recache"])

} else {

    // In the non-sandboxed case, we use the script on purpose as an extra sanity check (that the scripts have been
    // rewritten properly).
  let ghcPkgPath = location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent("ghc-pkg")
  ghcPkgTask = NSTask.launchedTaskWithLaunchPath(ghcPkgPath, arguments: ["recache"])

}
ghcPkgTask.waitUntilExit()
if ghcPkgTask.terminationStatus != 0 {
  NSLog("regenerating the binary package cache failed")
  exit(1)
}
