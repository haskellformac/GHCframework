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
/// NB: Needs to be a class inheriting from `NSObject` to be able to use the type in bindings to the preferences pane.
///
class Theme: NSObject {
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

  init(name:       String,
       background: NSColor,
       foreground: NSColor,
       keyword:    ThemeAttributes,
       keysymbol:  ThemeAttributes,
       varword:    ThemeAttributes,
       varsymbol:  ThemeAttributes,
       conword:    ThemeAttributes,
       consymbol:  ThemeAttributes,
       string:     ThemeAttributes,
       number:     ThemeAttributes,
       comment:    ThemeAttributes,
       pragma:     ThemeAttributes)
  {
    self.name = name
    self.foreground = foreground
    self.background = background
    self.keyword = keyword
    self.keysymbol = keysymbol
    self.varword = varword
    self.varsymbol = varsymbol
    self.conword = conword
    self.consymbol = consymbol
    self.string = string
    self.number = number
    self.comment = comment
    self.pragma = pragma
  }
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
let defaultThemes = [solarLight, monokai]

private let solarLight =
  Theme(name: "Solar Light",
        background: NSColor(SRGBRed: 255/255, green: 252/255, blue: 235/255, alpha: 1),
        foreground: NSColor.blackColor(),
        keyword:    solarLightKeyColour,
        keysymbol:  solarLightKeyColour,
        varword: ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        varsymbol: ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        conword: solarLightConColour,
        consymbol: solarLightConColour,
        string: ThemeAttributes(foreground: NSColor(SRGBRed: 223/255, green:   7/255, blue:   0/255, alpha: 1),
                                underline: false),
        number: ThemeAttributes(foreground: NSColor(SRGBRed:  41/255, green:  66/255, blue: 119/255, alpha: 1),
                                underline: false),
        comment: solarLightComColour,
        pragma: solarLightComColour)

private let solarLightKeyColour =
  ThemeAttributes(foreground: NSColor(SRGBRed:  41/255, green:  66/255, blue: 119/255, alpha: 1), underline: false)

private let solarLightConColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 180/255, green:  69/255, blue:   0/255, alpha: 1), underline: false)

private let solarLightComColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 195/255, green: 116/255, blue:  28/255, alpha: 1), underline: false)

private let monokai =
  Theme(name: "Monokai",
        background: NSColor(SRGBRed: 0x28/255, green: 0x28/255, blue: 0x28/255, alpha: 1),
        foreground: NSColor(SRGBRed: 0xf8/255, green: 0xf8/255, blue: 0xf2/255, alpha: 1),
        keyword:    monokaiKeyColour,
        keysymbol:  monokaiKeyColour,
        varword:    monokaiVarColour,
        varsymbol:  monokaiVarColour,
        conword:    monokaiConColour,
        consymbol:  monokaiConColour,
        string:     ThemeAttributes(foreground: NSColor(SRGBRed: 0xe6/255, green: 0xdb/255, blue: 0x74/255, alpha: 1),
                                    underline: false),
        number:     ThemeAttributes(foreground: NSColor(SRGBRed: 0xae/255, green: 0x81/255, blue: 0xff/255, alpha: 1),
                                    underline: false),
        comment:    monokaiComColour,
        pragma:     monokaiComColour)

private let monokaiKeyColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 0xf9/255, green: 0x26/255, blue: 0x72/255, alpha: 1), underline: false)

private let monokaiVarColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 0xf8/255, green: 0xf8/255, blue: 0xf2/255, alpha: 1), underline: false)

private let monokaiConColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 0xa6/255, green: 0xe2/255, blue: 0x2e/255, alpha: 1), underline: false)

private let monokaiComColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 0x75/255, green: 0x71/255, blue: 0x5e/255, alpha: 1), underline: false)
