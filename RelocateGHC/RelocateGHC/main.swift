//
//  main.swift
//  RelocateGHC
//
//  Created by Manuel M T Chakravarty on 12/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Rewrite the launch scripts and package database configuration files to the current location of the 'GHC.framework'.

import Foundation


// MARK: Helpers

infix operator ?! { }

// Abort if nil.
//
func ?!<A>(v: A?, msg: String) -> A {
  if let v = v {
    return v
  } else {
    println(msg)
    exit(1)
  }
}

// Replace all occurences of one string in a file by a new string.
//
func replaceStringWithString(inFile: String, string oldString: String, withString newString: String) {
  String(contentsOfFile: inFile)?.stringByReplacingOccurrencesOfString(oldString, withString: newString)
    .writeToFile(inFile, atomically: false, encoding: NSUTF8StringEncoding, error: nil)
                         // ^^^^if we'd do it atomically, we'd kill the executable permissions on the scripts
}


// MARK: Main

let location    = NSBundle.mainBundle().bundlePath.stringByAppendingPathComponent("..").stringByResolvingSymlinksInPath
let relativeBin = "usr/bin"
let relativeLib = "usr/lib/ghc-7.8.3"
let executables = ["ghc", "ghci", "ghc-pkg", "hpc", "hsc2hs", "runghc"]
let package_db  = location.stringByAppendingPathComponent(relativeLib).stringByAppendingPathComponent("package.conf.d")

let ghcPath     = location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent("ghc")
let ghcScript   = String(contentsOfFile: ghcPath)
                  ?! ("RelocateGHC: fatal error: could not load GHC launch script")

let startOfLoc  = ghcScript.rangeOfString("topdir=\"")?.endIndex
                  ?! ("RelocateGHC: fatal error: could not locate 'topdir' definition")

let endOfLoc    = ghcScript.rangeOfString("/usr/lib", options: nil, range: startOfLoc..<ghcScript.endIndex)?.startIndex
                  ?! ("RelocateGHC: fatal error: could not find end of old location")
let oldLocation = ghcScript[startOfLoc..<endOfLoc]

if location == oldLocation {
  exit(0)
}

let defaultFileManager = NSFileManager.defaultManager()

  // Rewrite the location in all executables.
for executable in
  (executables.map{ location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent($0) })
{
  replaceStringWithString(executable, string: oldLocation, withString: location)
}

let packagesDir = defaultFileManager.contentsOfDirectoryAtPath(package_db, error: nil)
  ?!  ("RelocateGHC: fatal error: could not read directory containing GHC package database")
let packages    = (packagesDir as? [String])!.filter{ $0.hasSuffix("conf") }.map{ package_db.stringByAppendingPathComponent($0) }
  ?! ("RelocateGHC: fatal error: could not load GHC package database")

  // Rewrite the location in all package 'conf' files.
for package in packages {
  replaceStringWithString(package, string: oldLocation, withString: location)
}

  // Refresh the package db cache.
let ghcPkgPath = location.stringByAppendingPathComponent(relativeBin).stringByAppendingPathComponent("ghc-pkg")
system(ghcPkgPath + " recache")
