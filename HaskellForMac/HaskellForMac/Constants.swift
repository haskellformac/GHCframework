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

extension Swift {
  class var swift_kPreferenceIndentationWidth:   String { get { return kPreferenceIndentationWidth } }
  class var swift_kPreferenceExternalTextEditor: String { get { return kPreferenceExternalTextEditor } }
}
