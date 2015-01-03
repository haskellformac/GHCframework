//
//  OS.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 27/12/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  OS-related utilities.

import Foundation


func isOperatingSystemAtLeastVersion10_10() -> Bool {
  if NSProcessInfo.processInfo().respondsToSelector("isOperatingSystemAtLeastVersion:") {

    let os10Version = NSOperatingSystemVersion(majorVersion: 10, minorVersion: 10, patchVersion: 0)
    return NSProcessInfo.processInfo().isOperatingSystemAtLeastVersion(os10Version)

  } else { return false }
}

extension Swift {
  class func swift_isOperatingSystemAtLeastVersion10_10() -> Bool {
    return isOperatingSystemAtLeastVersion10_10()
  }
}
