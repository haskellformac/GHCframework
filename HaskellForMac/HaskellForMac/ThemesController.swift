//
//  ThemesController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa


typealias FontChangeNotification  = NSFont -> ()
typealias ThemeChangeNotification = Theme -> ()

class ThemesController: NSController {

  @IBOutlet private weak var fontSizeTextField: NSTextField!
  @IBOutlet private weak var fontSizeStepper:   NSStepper!
  @IBOutlet private      var sampleCodeView:    NSTextView!

    // Bindings for the font selection: font popup button & combo box for the size
  dynamic var availableFonts:  [String] = ["Menlo-Regular"]
  dynamic var currentFontName: String   = "Menlo-Regular"
    { didSet { notifyFontChange(currentFont) } }
  dynamic var currentFontSize: Int      = 13
    { didSet { notifyFontChange(currentFont) } }

    // Bindings for the theme editor
  dynamic var themeNames:        [String] = defaultThemes.map{$0.name}
  dynamic var currentThemeIndex: Int      = 0

    // Definitive reference for the currently available themes.
  var themes: [Theme] = defaultThemes

    // Computed values.
  var currentFont: NSFont {
    get { return NSFont(name: currentFontName, size: CGFloat(currentFontSize))
                 ?? NSFont(name: "Menlo-Regular", size: 13)! }
  }
  var currentTheme: Theme {
    get {
      if currentThemeIndex >= themes.startIndex && currentThemeIndex < themes.endIndex {
        return themes[currentThemeIndex]
      } else {
        return defaultThemes[0]
      }
    }
  }

    /// Registered notifications.
    ///
    /// NB: The call back functions are weak references, they may go at any time, which implicitly unregisters them.
    ///
  private var fontChangeNotifications:  [WeakApply<FontChangeNotification>]  = []
  private var themeChangeNotifications: [WeakApply<ThemeChangeNotification>] = []


  /// Initialisation
  ///
  func setup() {

      // Font size bounds
    let smallestFontSize = 9
    if let formatter = fontSizeTextField.formatter as? NSNumberFormatter { formatter.minimum = smallestFontSize }
    fontSizeStepper.minValue = Double(smallestFontSize)

      // Get the initial list of available fonts.
    updateAvailableFonts()

      // Set up the the text view.
    sampleCodeView.font   = currentFont
    sampleCodeView.string = sampleCode
    registerFontNotification(self, curry{ ($0 as ThemesController).sampleCodeView.font = $1 }, themeChangeNotification: curry{ obj, theme in return })
  }

  /// Determine the list of available fixed pitch fonts and provide them by way of `availableFonts`.
  ///
  func updateAvailableFonts() {
    if let fonts = NSFontManager.sharedFontManager().availableFontNamesWithTraits(NSFontTraitMask.FixedPitchFontMask) {
      availableFonts = fonts as [String]
    }
  }

  /// Objects (e.g., code views) register to be notified of font and theme changes. The notification is automatically
  /// deregistered if the object gets deallocated.
  ///
  /// We keep a strong reference to the callback functions, but *not* to the object.
  ///
  func registerFontNotification<S: AnyObject>(object:                  S,
                                              fontChangeNotification:  S -> FontChangeNotification,
                                              themeChangeNotification: S -> ThemeChangeNotification)
  {
    fontChangeNotifications.append(WeakApply(fontChangeNotification, object))
    themeChangeNotifications.append(WeakApply(themeChangeNotification, object))
  }

  /// Invoke all register callbacks waiting for font changes.
  ///
  private func notifyFontChange(newFont: NSFont)
  {
    for notification in fontChangeNotifications {
      if let callback = notification.unbox { callback(newFont) }
    }

      // Prune stale notification callbacks.
    fontChangeNotifications = fontChangeNotifications.filter{ $0.unbox != nil }
  }
}

let sampleCode = "\n".join([ "map :: (a -> b) -> [a] -> [b]"
                           , "map f [] = []"
                           , "map f (x:xs) = f x : map f xs"
                           ])
