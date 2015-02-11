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
//class Theme {
  let name:       String
  let background: NSColor    // Background colour
  let foreground: NSColor    // Default foreground colour

    // Syntactic elements that may deviate from the default foreground colour
  let keyword:    ThemeAttributes    // Language-defined keyword
  let keysymbol:  ThemeAttributes    // Language-defined symbol
  let varword:    ThemeAttributes    // User-defined variable identifier
  let varsymbol:  ThemeAttributes    // User-defined variable symbol
  let conword:    ThemeAttributes    // User-defined constructor identifier
  let consymbol:  ThemeAttributes    // User-defined constructor symbol
  let string:     ThemeAttributes    // String or character constant
  let number:     ThemeAttributes    // Numeric constant
  let comment:    ThemeAttributes    // Single line or block comment
  let pragma:     ThemeAttributes    // Pragma specification
//  init(name:       String,
//       background: NSColor,
//       foreground: NSColor,
//       keyword:    ThemeAttributes,
//       keysymbol:  ThemeAttributes,
//       varword:    ThemeAttributes,
//       varsymbol:  ThemeAttributes,
//       conword:    ThemeAttributes,
//       consymbol:  ThemeAttributes,
//       string:     ThemeAttributes,
//       number:     ThemeAttributes,
//       comment:    ThemeAttributes,
//       pragma:     ThemeAttributes)
//  {
//    self.name = name
//    self.foreground = foreground
//    self.background = background
//    self.keyword = keyword
//    self.keysymbol = keysymbol
//    self.varword = varword
//    self.varsymbol = varsymbol
//    self.conword = conword
//    self.consymbol = consymbol
//    self.string = string
//    self.number = number
//    self.comment = comment
//    self.pragma = pragma
//  }
}

/// The attributes that are determined by a theme for each syntactic element.
///
struct ThemeAttributes {
//class ThemeAttributes {
  let foreground: NSColor
  let underline:  Bool
//  init(foreground: NSColor, underline:  Bool) { self.foreground = foreground; self.underline = underline }
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
        varword: ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        varsymbol: ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        conword: solarizedLightConColour,
        consymbol: solarizedLightConColour,
        string: ThemeAttributes(foreground: NSColor(calibratedRed: 223/255, green:   7/255, blue:   0/255, alpha: 1),
                                underline: false),
        number: ThemeAttributes(foreground: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1),
                                underline: false),
        comment: solarizedLightComColour,
        pragma: solarizedLightComColour)

private let solarizedLightKeyColour =
  ThemeAttributes(foreground: NSColor(calibratedRed:  41/255, green:  66/255, blue: 119/255, alpha: 1), underline: false)

private let solarizedLightConColour =
  ThemeAttributes(foreground: NSColor(calibratedRed: 180/255, green:  69/255, blue:   0/255, alpha: 1), underline: false)

private let solarizedLightComColour =
  ThemeAttributes(foreground: NSColor(calibratedRed: 195/255, green: 116/255, blue:  28/255, alpha: 1), underline: false)

private let monokai = solarizedLight
