//
//  Theme.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


extension NSUserDefaults {

  func setColor(color: NSColor, forKey key: String) {
    setObject(NSArchiver.archivedDataWithRootObject(color), forKey:key)
  }

  func setThemes(themes: [Theme], forKey key: String) {
    let themeArray = themes.map{themeToDictionary($0)}
    setObject(themeArray, forKey: key)
  }

  func colorForKey(key: NSString) -> NSColor? {
    if let data = dataForKey(key) {
      return NSUnarchiver.unarchiveObjectWithData(data) as? NSColor
    } else { return nil }
  }

  func themesForKey(key: NSString) -> [Theme] {
    if let dicts = objectForKey(key) as? [[String: AnyObject]] {
      return dicts.map{dictionaryToTheme($0)}
    } else { return defaultThemes }
  }
}


/// Text properties varying by theme.
///
/// NB: Needs to be a class inheriting from `NSObject` to be able to use the type in bindings to the preferences pane.
///
public struct Theme {
  var name:       String
  var foreground: NSColor    // Default foreground colour
  var background: NSColor    // Background colour
  var invisibles: NSColor
  var cursor    : NSColor
  var selection : NSColor

    // Syntactic elements that may deviate from the default foreground colour
  var keyword:    ThemeAttributes    // Language-defined keyword
  var keysymbol:  ThemeAttributes    // Language-defined symbol
  var varword:    ThemeAttributes    // User-defined variable identifier
  var varsymbol:  ThemeAttributes    // User-defined variable symbol
  var conword:    ThemeAttributes    // User-defined constructor identifier
  var consymbol:  ThemeAttributes    // User-defined constructor symbol
  var string:     ThemeAttributes    // String constant
  var char:       ThemeAttributes    // Character constant
  var number:     ThemeAttributes    // Numeric constant
  var comment:    ThemeAttributes    // Single line or block comment
  var pragma:     ThemeAttributes    // Pragma specification
}

extension Theme: Equatable {}

public func ==(lhs: Theme, rhs: Theme) -> Bool {

  // FIXME: Broken up for swiftc's type checker's benefit *sigh* (Xcode 6.1.1)
  let firstPart = lhs.name == rhs.name && lhs.foreground <==> rhs.foreground && lhs.background <==> rhs.background &&
    lhs.invisibles <==> rhs.invisibles && lhs.cursor <==> rhs.cursor && lhs.selection <==> rhs.selection
  let secondPart = lhs.keyword == rhs.keyword && lhs.keysymbol == rhs.keysymbol && lhs.varword == rhs.varword &&
    lhs.varsymbol == rhs.varsymbol && lhs.conword == rhs.conword && lhs.consymbol == rhs.consymbol
  return  firstPart && secondPart &&
    lhs.string == rhs.string && lhs.char == rhs.char && lhs.number == rhs.number && lhs.comment == rhs.comment &&
    lhs.pragma == rhs.pragma
}

public func themeToDictionary(theme: Theme) -> [String: AnyObject] {
  return Dictionary(dictionaryLiteral: ("name",       theme.name)
                                     , ("foreground", NSArchiver.archivedDataWithRootObject(theme.foreground))
                                     , ("background", NSArchiver.archivedDataWithRootObject(theme.background))
                                     , ("invisibles", NSArchiver.archivedDataWithRootObject(theme.invisibles))
                                     , ("cursor",     NSArchiver.archivedDataWithRootObject(theme.cursor))
                                     , ("selection",  NSArchiver.archivedDataWithRootObject(theme.selection))
                                     , ("keyword",    themeAttributesToDictionary(theme.keyword))
                                     , ("keysymbol",  themeAttributesToDictionary(theme.keysymbol))
                                     , ("varword",    themeAttributesToDictionary(theme.varword))
                                     , ("varsymbol",  themeAttributesToDictionary(theme.varsymbol))
                                     , ("conword",    themeAttributesToDictionary(theme.conword))
                                     , ("consymbol",  themeAttributesToDictionary(theme.consymbol))
                                     , ("string",     themeAttributesToDictionary(theme.string))
                                     , ("char",       themeAttributesToDictionary(theme.char))
                                     , ("number",     themeAttributesToDictionary(theme.number))
                                     , ("comment",    themeAttributesToDictionary(theme.comment))
                                     , ("pragma",     themeAttributesToDictionary(theme.pragma))
                                     )
}

public func dictionaryToTheme(dict: [String: AnyObject]) -> Theme {
  let name       = (dict["name"] as? String) ?? "No Name"
  let foreground = unarchiveNSColor(dict["foreground"])
  let background = unarchiveNSColor(dict["background"])
  let invisibles = unarchiveNSColor(dict["invisibles"])
  let cursor     = unarchiveNSColor(dict["cursor"])
  let selection  = unarchiveNSColor(dict["selection"])
  let keyword    = dictionaryToThemeAttributes((dict["keyword"] as? [String: AnyObject]) ?? [:])
  let keysymbol  = dictionaryToThemeAttributes((dict["keysymbol"] as? [String: AnyObject]) ?? [:])
  let varword    = dictionaryToThemeAttributes((dict["varword"] as? [String: AnyObject]) ?? [:])
  let varsymbol  = dictionaryToThemeAttributes((dict["varsymbol"] as? [String: AnyObject]) ?? [:])
  let conword    = dictionaryToThemeAttributes((dict["conword"] as? [String: AnyObject]) ?? [:])
  let consymbol  = dictionaryToThemeAttributes((dict["consymbol"] as? [String: AnyObject]) ?? [:])
  let string     = dictionaryToThemeAttributes((dict["string"] as? [String: AnyObject]) ?? [:])
  let char       = dictionaryToThemeAttributes((dict["char"] as? [String: AnyObject]) ?? [:])
  let number     = dictionaryToThemeAttributes((dict["number"] as? [String: AnyObject]) ?? [:])
  let comment    = dictionaryToThemeAttributes((dict["comment"] as? [String: AnyObject]) ?? [:])
  let pragma     = dictionaryToThemeAttributes((dict["pragma"] as? [String: AnyObject]) ?? [:])
  return Theme( name: name
              , foreground: foreground
              , background: background
              , invisibles: invisibles
              , cursor: cursor
              , selection: selection
              , keyword: keyword
              , keysymbol: keysymbol
              , varword: varword
              , varsymbol: varsymbol
              , conword: conword
              , consymbol: consymbol
              , string: string
              , char: char
              , number: number
              , comment: comment
              , pragma: pragma
              )
}

/// The attributes that are determined by a theme for each syntactic element.
///
public struct ThemeAttributes {
  var foreground: NSColor
  var underline:  Bool

  // Explicit as we otherwise cannot use it in another module...
  public init(foreground: NSColor, underline: Bool) {self.foreground = foreground; self.underline = underline}
}

extension ThemeAttributes: Equatable {}

public func ==(lhs: ThemeAttributes, rhs: ThemeAttributes) -> Bool {
  return lhs.foreground <==> rhs.foreground && lhs.underline == rhs.underline
}

infix operator <==> { precedence 130 }

func <==>(lhs: NSColor, rhs: NSColor) -> Bool {
  return lhs.colorUsingColorSpace(NSColorSpace.genericRGBColorSpace()) == rhs.colorUsingColorSpace(NSColorSpace.genericRGBColorSpace())
}

public func themeAttributesToDictionary(attributes: ThemeAttributes) -> [String: AnyObject] {
  return Dictionary(dictionaryLiteral: ("foreground", NSArchiver.archivedDataWithRootObject(attributes.foreground))
                                     , ("underline",  attributes.underline))
}

public func dictionaryToThemeAttributes(dict: [String: AnyObject]) -> ThemeAttributes {
  let foreground = unarchiveNSColor(dict["foreground"])
  let underline  = (dict["underline"] as? Bool) ?? false
  return ThemeAttributes(foreground: foreground, underline: underline)
}

func unarchiveNSColor(obj: AnyObject?) -> NSColor {
  if let data = obj as? NSData {
    return (NSUnarchiver.unarchiveObjectWithData(data) as? NSColor) ?? NSColor.whiteColor()
  } else { return NSColor.whiteColor() }
}

/// Derive a gutter colour from the background colour of a theme.
///
func gutterColour(theme: Theme) -> NSColor {
  let backgroundColour = theme.background
  if backgroundColour.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace())?.brightnessComponent >= 0.5 {
    if let resultBackgroundColour = backgroundColour.shadowWithLevel(0.05) { return resultBackgroundColour }
    else { return backgroundColour }
  } else {
    if let resultBackgroundColour = backgroundColour.highlightWithLevel(0.09) { return resultBackgroundColour }
    else { return backgroundColour }
  }
}

/// Derive a divider colour from the background colour of a theme.
///
func dividerColour(theme: Theme) -> NSColor {
  let backgroundColour = theme.background
  if backgroundColour.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace())?.brightnessComponent >= 0.5 {
    if let resultBackgroundColour = backgroundColour.shadowWithLevel(0.12) { return resultBackgroundColour }
    else { return backgroundColour }
  } else {
    if let resultBackgroundColour = backgroundColour.highlightWithLevel(0.20) { return resultBackgroundColour }
    else { return backgroundColour }
  }
}

/// Derive a disbaled variant of the foreground colour of a theme.
///
func disabledForegroundColour(theme: Theme) -> NSColor {
  let foregroundColour = theme.foreground
  if foregroundColour.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace())?.brightnessComponent >= 0.5 {
    if let resultForegroundColour = foregroundColour.shadowWithLevel(0.35) { return resultForegroundColour }
    else { return foregroundColour }
  } else {
    if let resultForegroundColour = foregroundColour.highlightWithLevel(0.4) { return resultForegroundColour }
    else { return foregroundColour }
  }
}


// MARK: -
// MARK: Default themes

/// Initial set of themes.
///
/// The first theme in this array is the default template for creating new themes.
///
public let defaultThemes = [plain, inverse, solarLight, monokai]

private let plain =
  Theme(name: "Plain",
        foreground: NSColor.blackColor(),
        background: NSColor.whiteColor(),
        invisibles: NSColor.lightGrayColor(),
        cursor:     NSColor.blackColor(),
        selection:  NSColor.highlightColor(),
        keyword:    ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        keysymbol:  ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        varword:    ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        varsymbol:  ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        conword:    ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        consymbol:  ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        string:     ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        char:       ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        number:     ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        comment:    ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        pragma:     ThemeAttributes(foreground: NSColor.blackColor(), underline: false))

private let inverse =
  Theme(name: "Inverse",
        foreground: NSColor.whiteColor(),
        background: NSColor.blackColor(),
        invisibles: NSColor.darkGrayColor(),
        cursor:     NSColor.whiteColor(),
        selection:  NSColor.highlightColor(),
        keyword:    ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        keysymbol:  ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        varword:    ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        varsymbol:  ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        conword:    ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        consymbol:  ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        string:     ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        char:       ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        number:     ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        comment:    ThemeAttributes(foreground: NSColor.whiteColor(), underline: false),
        pragma:     ThemeAttributes(foreground: NSColor.whiteColor(), underline: false))

private let solarLight =
  Theme(name: "Solar Light",
        foreground: NSColor.blackColor(),
        background: NSColor(SRGBRed: 255/255, green: 252/255, blue: 235/255, alpha: 1),
        invisibles: NSColor(SRGBRed: 230/255, green: 227/255, blue: 212/255, alpha: 1),
        cursor:     NSColor.blackColor(),
        selection:  NSColor(SRGBRed: 250/255, green: 227/255, blue: 175/255, alpha: 1),
        keyword:    solarLightKeyColour,
        keysymbol:  solarLightKeyColour,
        varword:    ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        varsymbol:  ThemeAttributes(foreground: NSColor.blackColor(), underline: false),
        conword:    solarLightConColour,
        consymbol:  solarLightConColour,
        string:     ThemeAttributes(foreground: NSColor(SRGBRed: 223/255, green:   7/255, blue:   0/255, alpha: 1),
                                    underline: false),
        char:       ThemeAttributes(foreground: NSColor(SRGBRed:  55/255, green:  87/255, blue: 136/255, alpha: 1),
                                    underline: false),
        number:     ThemeAttributes(foreground: NSColor(SRGBRed:  41/255, green:  66/255, blue: 119/255, alpha: 1),
                                    underline: false),
        comment:    solarLightComColour,
        pragma:     solarLightComColour)

private let solarLightKeyColour =
  ThemeAttributes(foreground: NSColor(SRGBRed:  41/255, green:  66/255, blue: 119/255, alpha: 1), underline: false)

private let solarLightConColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 180/255, green:  69/255, blue:   0/255, alpha: 1), underline: false)

private let solarLightComColour =
  ThemeAttributes(foreground: NSColor(SRGBRed: 195/255, green: 116/255, blue:  28/255, alpha: 1), underline: false)

private let monokai =
  Theme(name: "Monokai",
        foreground: NSColor(SRGBRed: 0xf8/255, green: 0xf8/255, blue: 0xf2/255, alpha: 1),
        background: NSColor(SRGBRed: 0x28/255, green: 0x28/255, blue: 0x28/255, alpha: 1),
        invisibles: NSColor(SRGBRed:  197/255, green:  200/255, blue:  198/255, alpha: 0.2),
        cursor:     NSColor(SRGBRed: 0xf8/255, green: 0xf8/255, blue: 0xf0/255, alpha: 1),
        selection:  NSColor(SRGBRed: 0x49/255, green: 0x48/255, blue: 0x3e/255, alpha: 1),
        keyword:    monokaiKeyColour,
        keysymbol:  monokaiKeyColour,
        varword:    monokaiVarColour,
        varsymbol:  monokaiVarColour,
        conword:    monokaiConColour,
        consymbol:  monokaiConColour,
        string:     ThemeAttributes(foreground: NSColor(SRGBRed: 0xe6/255, green: 0xdb/255, blue: 0x74/255, alpha: 1),
                                    underline: false),
        char:       ThemeAttributes(foreground: NSColor(SRGBRed: 0xae/255, green: 0x81/255, blue: 0xff/255, alpha: 1),
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
