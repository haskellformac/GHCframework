//
//  Constants.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 18/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Constants used throughout the app.

import Foundation

@objc class Swift { }     // for bridging

let kPreferenceIndentationWidth   = "IndentationWidth"
let kPreferenceExternalTextEditor = "ExternalTextEditor"
let kPreferenceFontName           = "FontName"
let kPreferenceFontSize           = "FontSize"
let kPreferenceThemes             = "Themes"
let kPreferenceThemeName          = "ThemeName"
let kPreferenceEnableCloud        = "EnableCloud"
let kPreferenceUsername           = "Username"

  // Debug preferences
let kPreferenceChangesLogLevel    = "ChangesLogLevel"         // Must match the key used by 'TacticalBase'.
let kPreferenceDeinitLogLevel     = "DeinitLogLevel"
let kPreferenceGHCLogLevel        = "GHCLogLevel"             // Must match the key used by 'GHCKit'.
let kPreferenceSpriteKitLogLevel  = "SpriteKitLogLevel"
let kPreferenceCloudLogLevel      = "CloudLogLevel"           // Must match the key used by 'CloudcelerateKit'.

extension Swift {
  class var swift_kPreferenceIndentationWidth:   String { get { return kPreferenceIndentationWidth } }
  class var swift_kPreferenceExternalTextEditor: String { get { return kPreferenceExternalTextEditor } }
  class var swift_kPreferenceFontName:           String { get { return kPreferenceFontName } }
  class var swift_kPreferenceFontSize:           String { get { return kPreferenceFontSize } }
  class var swift_kPreferenceThemes:             String { get { return kPreferenceThemes } }
  class var swift_kPreferenceThemeName:          String { get { return kPreferenceThemeName } }
  class var swift_kPreferenceEnableCloud:        String { get { return kPreferenceEnableCloud } }
  class var swift_kPreferenceUsername:           String { get { return kPreferenceUsername } }

  class var swift_kPreferenceChangesLogLevel:    String { get { return kPreferenceChangesLogLevel } }
  class var swift_kPreferenceDeinitLogLevel:     String { get { return kPreferenceDeinitLogLevel } }
  class var swift_kPreferenceGHCLogLevel:        String { get { return kPreferenceGHCLogLevel } }
  class var swift_kPreferenceSpriteKitLogLevel:  String { get { return kPreferenceSpriteKitLogLevel } }
  class var swift_kPreferenceCloudLogLevel:      String { get { return kPreferenceCloudLogLevel } }
}
