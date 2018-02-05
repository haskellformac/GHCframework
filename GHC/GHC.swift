//
//  GHC.swift
//  GHC
//
//  Created by Manuel M T Chakravarty on 29/07/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  This class provides information about the embedded system. We define it as a class, so that this bundle contains
//  a class, and hence, gets listed in framework bundle queries.

import Cocoa

final public class GHC: NSObject {

  /// Determine the GHC version, LTS version, and framework version.
  ///
  /// NB: The returned strings still need to be tested for compliance to the versioning conventions of the subcomponent
  ///     they refer to.
  ///
  public class var haskellVersion: (String, String, String) { get {
    let bundle        = Bundle(for: GHC.self),
        bundleVersion = (bundle.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "unknown",
        components    = bundleVersion.split(omittingEmptySubsequences: false){ $0 == "-" }.map { String($0) }
    if components.count == 3 {
      return (components[0], components[1], components[2])
    } else { return ("", "", bundleVersion) }
    } }
}
