//
//  Constants.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 18/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Constants used throughout the app.

import Foundation

@objc class Swift { }     // for briding

let kPreferenceIndentationWidth   = "IndentationWidth"
let kPreferenceExternalTextEditor = "ExternalTextEditor"
let kPreferenceEnableCloud        = "EnableCloud"
let kPreferenceGHCLogLevel        = "GHCLogLevel"
let kPreferenceSpriteKitLogLevel  = "SpriteKitLogLevel"
let kPreferenceCloudLogLevel      = "CloudLogLevel"

extension Swift {
  class var swift_kPreferenceIndentationWidth:   String { get { return kPreferenceIndentationWidth } }
  class var swift_kPreferenceExternalTextEditor: String { get { return kPreferenceExternalTextEditor } }
  class var swift_kPreferenceEnableCloud:        String { get { return kPreferenceEnableCloud } }
  class var swift_kPreferenceGHCLogLevel:        String { get { return kPreferenceGHCLogLevel } }
  class var swift_kPreferenceSpriteKitLogLevel:  String { get { return kPreferenceSpriteKitLogLevel } }
  class var swift_kPreferenceCloudLogLevel:      String { get { return kPreferenceCloudLogLevel } }
}
