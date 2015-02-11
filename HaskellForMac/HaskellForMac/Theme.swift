//
//  Theme.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


/// Text properties varying by theme.
///
struct Theme {
  let name:       String
  let background: NSColor    // Background colour
  let foreground: NSColor    // Default foreground colour

    // Syntactic elements that may deviate from the default foreground colour
  let keyword:    NSColor    // Language-defined keyword
  let keysymbol:  NSColor    // Language-defined symbol
  let varword:    NSColor    // User-defined variable identifier
  let varsymbol:  NSColor    // User-defined variable symbol
  let conword:    NSColor    // User-defined constructor identifier
  let consymbol:  NSColor    // User-defined constructor symbol
  let string:     NSColor    // String or character constant
  let number:     NSColor    // Numeric constant
  let comment:    NSColor    // Single line or block comment
  let pragma:     NSColor    // Pragma specification
}

/// The attributes that are determined by a theme for each syntactic element.
///
struct ThemeAttributes {
  let foreground: NSColor
  let underline:  Bool
}

/// Initial set of themes.
///
let defaultThemes = [solarizedLight, monokai]

private let solarizedLight =
  Theme(name: "Solarized Light",
        background: NSColor(calibratedRed: 255/255, green: 252/255, blue: 235/255, alpha: 1),
        foreground: NSColor.blackColor(),
        keyword: solarizedLightKeyColour,
        keysymbol: solarizedLightKeyColour,
        varword: NSColor.blackColor(),
        varsymbol: NSColor.blackColor(),
        conword: solarizedLightConColour,
        consymbol: solarizedLightConColour,
        string: NSColor(calibratedRed: 223/255, green:   7/255, blue:   0/255, alpha: 1),
        number: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1),
        comment: solarizedLightComColour,
        pragma: solarizedLightComColour)

private let solarizedLightKeyColour =
  NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1)

private let solarizedLightConColour =
  NSColor(calibratedRed: 180/255, green:  69/255, blue:   0/255, alpha: 1)

private let solarizedLightComColour =
  NSColor(calibratedRed: 195/255, green: 116/255, blue:  28/255, alpha: 1)

private let monokai = solarizedLight
