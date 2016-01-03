//
//  main.swift
//  RelocateGHC
//
//  Created by Manuel M T Chakravarty on 12/09/2014.
//  Copyright (c) [2014..2016] Manuel M T Chakravarty. All rights reserved.
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
         endOfLoc    = conf.rangeOfString("/usr/lib", options: [], range: startOfLoc..<conf.endIndex)?.startIndex
  {
    return conf[startOfLoc..<endOfLoc]
  } else { return nil }
}

// Replace all occurences of one string in a file by a new string, possibly writing the result to a new file
//
func replaceInFile(source: String, string oldString: String, withString newString: String, writingTo target: String? = nil)
{
  do {
    try String(contentsOfFile: source).stringByReplacingOccurrencesOfString(oldString, withString: newString)
      .writeToFile(target ?? source, atomically: false, encoding: NSUTF8StringEncoding)
  } catch _ {
  }
                                   // ^^^^if we'd do it atomically, we'd kill the executable permissions on the scripts
}


// MARK: Determine target location

let bundleLocation = NSURL(fileURLWithPath: NSBundle.mainBundle().bundlePath).URLByAppendingPathComponent("..").URLByResolvingSymlinksInPath!
let location       = bundleLocation.path!.hasSuffix("GHC.framework/Versions/A") // NB: need to use 'A', not 'Current', as this is canonical
                     ? bundleLocation
                     : NSURL(fileURLWithPath: NSFileManager.defaultManager().currentDirectoryPath).URLByAppendingPathComponent("GHC.framework/Versions/A")

let relativeBin   = "usr/bin"
let relativeLib   = "usr/lib/ghc"
let executables   = ["ghc", "ghci", "ghc-pkg", "hpc", "hsc2hs", "runghc"]
let packageDBPath = location.URLByAppendingPathComponent(relativeLib).URLByAppendingPathComponent("package.conf.d")

let rtsConfPath   = packageDBPath.URLByAppendingPathComponent("builtin_rts.conf")
let rtsConfData   = (try? String(contentsOfFile: rtsConfPath.path!))
                    ?! ("fatal error: could not load RTS package configuration \(rtsConfPath)")

let oldLocation   = libraryLocation(rtsConfData)
                    ?! ("fatal error: could not extract library path from 'library-dirs' field")

if location == NSURL(fileURLWithPath: oldLocation) {
  exit(0)  // location is already up to date
}
print("Relocating from \(oldLocation) to \(location.path!)")


// MARK: Process arguments

let defaultFileManager = NSFileManager.defaultManager()

var appContainerGHCRoot      : NSURL? = nil
var appContainerGHCLib       : NSURL? { get {
  return appContainerGHCRoot?.URLByAppendingPathComponent("lib/ghc")
} }
var appContainerPackageDBPath: NSURL? { get {
  return appContainerGHCLib?.URLByAppendingPathComponent("package.conf.d")
} }

if Process.argc == 3 && Process.arguments[1] == "--sandboxed" {

  appContainerGHCRoot = NSURL(fileURLWithPath: Process.arguments[2])

    // We need to check both the location as well as the time stamp, as we may switch between different copies as well
    // as update one to a newer version. NB: This is still not entirely safe, as we may overwrite one version with an
    // older one — but that should only happen during testing (when we have to manually remove the app container DB).
  let appContainerRtsConfPath    = appContainerPackageDBPath!.URLByAppendingPathComponent("builtin_rts.conf")
    // FIXME: the following needs to have error handling cleaned up
  let str: String? = try? String(contentsOfFile: appContainerRtsConfPath.path!)
  if let appContainerRtsConfData = str,
//  if let appContainerRtsConfData = try String(contentsOfFile: appContainerRtsConfPath.path!),
         appContainerLocation    = libraryLocation(appContainerRtsConfData)    // library location used in app container DB
  {
    if NSURL(fileURLWithPath: appContainerLocation) == location {

      let embeddedCachePath      = packageDBPath.URLByAppendingPathComponent("package.cache"),
          embeddedCacheAttrs     = try? defaultFileManager.attributesOfItemAtPath(embeddedCachePath.path!),
          appContainerCachePath  = appContainerPackageDBPath!.URLByAppendingPathComponent("package.cache"),
          appContainerCacheAttrs = try? defaultFileManager.attributesOfItemAtPath(appContainerCachePath.path!)
      if let embeddedCacheDate      = (embeddedCacheAttrs as NSDictionary?)?.fileModificationDate(),
              appContainerCacheDate = (appContainerCacheAttrs as NSDictionary?)?.fileModificationDate()
        where appContainerCacheDate.compare(embeddedCacheDate) == .OrderedDescending
      {
        exit(0)  // location and timestamp of app-container package-db are up to date
      }
    }

      // Remove the out of date package database as a whole. (The new one may have different package sets, versions,
      // or package IDs, in which case, droppings of the old may remain if we just write over the old DB.)
    print("Removing old package DB in app container")
    if let packageDBPath = appContainerPackageDBPath {

      do { try defaultFileManager.removeItemAtPath(packageDBPath.path!) }
      catch let error {
        NSLog("failed to remove existing file at package DB location in app container: \(error)")
        exit(1)
      }

    } else {

      NSLog("failed to remove existing file at package DB location in app container: no container path")
      exit(1)

    }
  }
  print("Updating package DB in app container")

}

var sandboxed: Bool { get { return appContainerPackageDBPath != nil } }


// MARK: Start rewriting

  // Rewrite the location in all executables if not in sandboxed mode.
if !sandboxed {
  for executable in
    (executables.map{ location.URLByAppendingPathComponent(relativeBin).URLByAppendingPathComponent($0) })
  {
    replaceInFile(executable.path!, string: oldLocation, withString: location.path!)
  }
}

let packagesDir = (try? defaultFileManager.contentsOfDirectoryAtPath(packageDBPath.path!))
                  ?!  ("fatal error: could not read directory containing GHC package database at \(packageDBPath)")
let packages    = packagesDir.filter{ $0.hasSuffix("conf") }
                  ?! ("fatal error: could not load GHC package database")

  // In sandboxed mode, create the package DB in the app container if it doesn't exit yet.
if let packageDBPath = appContainerPackageDBPath {

  var error:       NSError?
  var isDirectory: ObjCBool = false
  let package_conf_d_exists = defaultFileManager.fileExistsAtPath(packageDBPath.path!, isDirectory: &isDirectory)
  if !package_conf_d_exists {                         // doesn't exist => create directory

    do {
      try defaultFileManager.createDirectoryAtPath(packageDBPath.path!,
                                                       withIntermediateDirectories: true,
                                                       attributes: nil)
    } catch var error1 as NSError {
      error = error1
      NSLog("failed to create package DB in app container: %@", error!)
      exit(1)
    }

  } else if !isDirectory {                            // plain file exists => remove & create directory

    do {
      try defaultFileManager.removeItemAtPath(packageDBPath.path!)
    } catch var error1 as NSError {
      error = error1
      NSLog("failed to remove existing file at package DB location in app container: %@", error!)
      exit(1)
    }
    do {
      try defaultFileManager.createDirectoryAtPath(packageDBPath.path!,
                                                       withIntermediateDirectories: true,
                                                       attributes: nil)
    } catch var error1 as NSError {
      error = error1
      NSLog("failed to create package DB in app container: %@", error!)
      exit(1)
    }

  }
}

  // Rewrite the location in all package 'conf' files, writing to the app container in sandboxed mode.
for package in packages {

  let source = packageDBPath.URLByAppendingPathComponent(package),
      target = appContainerPackageDBPath?.URLByAppendingPathComponent(package)
  replaceInFile(source.path!, string: oldLocation, withString: location.path!, writingTo: target?.path)

}

  // Refresh the package db cache.
let ghcPkgTask: NSTask
if let packageDBPath = appContainerPackageDBPath {

  let ghcPkgPath = location.URLByAppendingPathComponent("Executables/ghc-pkg")
  ghcPkgTask = NSTask.launchedTaskWithLaunchPath(ghcPkgPath.path!, arguments: ["--global-package-db", packageDBPath.path!, "recache"])

} else {

    // In the non-sandboxed case, we use the script on purpose as an extra sanity check (that the scripts have been
    // rewritten properly).
  let ghcPkgPath = location.URLByAppendingPathComponent(relativeBin).URLByAppendingPathComponent("ghc-pkg")
  ghcPkgTask = NSTask.launchedTaskWithLaunchPath(ghcPkgPath.path!, arguments: ["recache"])

}
ghcPkgTask.waitUntilExit()
if ghcPkgTask.terminationStatus != 0 {
  NSLog("regenerating the binary package cache failed")
  exit(1)
}
