//
//  ThemesController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 10/02/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa


let defaultFontName: String = "Menlo-Regular"
let defaultFontSize: Int    = 13

typealias FontChangeNotification  = NSFont -> ()
typealias ThemeChangeNotification = Theme -> ()

  // Identifiers for buttons in the preferences pane
private let kThemeListAdd    = "ThemeListAdd"
private let kThemeListAction = "ThemeListAction"

private enum TokenSelection: Printable {
  case NoSelection
  case Selection(token: HighlightingTokenKind)
  case AmbiguousSelection

  var highlightingTokenKind: HighlightingTokenKind? {
    get {
      switch self {
      case .Selection(let tokenKind): return tokenKind
      default:                        return nil
      }
    }
  }

  var description: String {
    get {
      switch self {
      case .NoSelection:                      return ""
      case .Selection(token: let tokenKind0): let tokenKind = tokenKind0
      return tokenKind.description
      case .AmbiguousSelection:               return "«Ambiguous selection»"
      }
    }
  }
}

class ThemesController: NSController {

    // Available once the preferences window has been loaded (for access to the views of the theme tab)
  weak var preferencesController: PreferencesController?

    // Bindings for the font selection: font popup button & combo box for the size.
    //
    // NB: We need to persist to defaults here as we only want to set the name or size depending on what changed; 
    //     otherwise, we'll get excessive change notifications.
  dynamic var availableFonts:  [String] = [defaultFontName]
  dynamic var currentFontName: String   = defaultFontName
    { didSet {
      if oldValue != currentFontName {
        notifyFontChange(currentFont)
        NSUserDefaults.standardUserDefaults().setObject(currentFontName, forKey: kPreferenceFontName)
      }
    } }
  dynamic var currentFontSize: Int      = defaultFontSize
    { didSet {
      if oldValue != currentFontSize {
        notifyFontChange(currentFont)
        NSUserDefaults.standardUserDefaults().setObject(currentFontSize, forKey: kPreferenceFontSize)
      }
    } }

    // Bindings for the theme editor. (Initial values will be overridden when themes preferences are read.)
  dynamic var themeNames:          [String]   = []
  dynamic var currentThemeIndexes: NSIndexSet = NSIndexSet()
    { didSet {
      if currentThemeIndexes.count > 0 { notifyThemeChange(currentTheme) }
    } }

    /// Definitive reference for the currently available themes — the model as far as themes are concerned.
    /// NB: Initialised by the `PreferencesController`.
  var themes: [String: Theme] = [:]
    { didSet {
      if themes.count > 0 && currentThemeIndexes.count > 0 {  // Only after initialisation is finished!
        notifyThemeChange(currentTheme)                                             // Update UI
        let userDefaults = NSUserDefaults.standardUserDefaults()
        userDefaults.setThemes(Array(themes.values), forKey: kPreferenceThemes)     // Archive to NSUserDefaults
      }
    } }

    /// The token category currently selected for editing if any.
  var selectedTokenKind: HighlightingTokenKind?

    // Computed values.
  var currentFont: NSFont {
    get { return NSFont(name: currentFontName, size: CGFloat(currentFontSize))
                 ?? NSFont(name: defaultFontName, size: CGFloat(defaultFontSize))! }
  }
  var currentThemeName: String {
    get {
      let currentThemeIndex = currentThemeIndexes.count == 1 ? currentThemeIndexes.firstIndex : 0
      if currentThemeIndex >= themeNames.startIndex && currentThemeIndex < themeNames.endIndex {
        return themeNames[currentThemeIndex] ?? ""
      } else {
        return ""
      }
    }
  }
  var currentTheme: Theme {
    get { return themes[currentThemeName] ?? defaultThemes[0] }
  }

  /// Registered notifications.
  ///
  /// NB: The call back functions are weak references, they may go at any time, which implicitly unregisters them.
  ///
  private var fontChangeNotifications:  [WeakApply<FontChangeNotification>]  = []
  private var themeChangeNotifications: [WeakApply<ThemeChangeNotification>] = []

  /// We need to keep the code storage delegate alive as the delegate reference from `NSTextStorage` is unowned.
  ///
  private var codeStorageDelegate: CodeStorageDelegate!

  /// Ugly hack — see `setup()`.
  private var initialThemeName: String = ""


  // MARK: -
  // MARK: Shared controller

  /// Get the themes controller from the main menu nib.
  ///
  class func sharedThemesController() -> ThemesController {
    return ((NSApp as NSApplication).delegate as AppDelegate).themesController
  }


  // MARK: -
  // MARK: Controller configuration

  override func awakeFromNib() {
    super.awakeFromNib()

      // Load themes — we need them independent of whether or not the preferences window is going to be used.
    let initialThemes              = NSUserDefaults.standardUserDefaults().themesForKey(kPreferenceThemes)
    var theThemes: [String: Theme] = [:]
    for theme in initialThemes {theThemes.updateValue(theme, forKey: theme.name)}
    themes = theThemes

      // Initialise themes data for the table view with theme names.
    themeNames = Array(themes.keys).sorted(<=)
    if let let themeName = NSUserDefaults.standardUserDefaults().stringForKey(kPreferenceThemeName) {
      initialThemeName = themeName
      let themeIndex = (themeNames as NSArray).indexOfObject(themeName)
      if themeIndex != NSNotFound { currentThemeIndexes = NSIndexSet(index: themeIndex) }
      else { currentThemeIndexes = NSIndexSet(index: 0) }
    } else { currentThemeIndexes = NSIndexSet(index: 0) }

      // Register for changes in user defaults.
    NSNotificationCenter.defaultCenter().addObserver(self,
                                                     selector: "userDefaultsDidChange:",
                                                     name: NSUserDefaultsDidChangeNotification,
                                                     object: NSUserDefaults.standardUserDefaults())
  }

  /// Initialisation after the preferences window has been loaded.
  ///
  func setup(preferencesController: PreferencesController) {

    self.preferencesController = preferencesController

      // Font size bounds
    let smallestFontSize = 9
    if let formatter = preferencesController.fontSizeTextField.formatter as? NSNumberFormatter {
      formatter.minimum = smallestFontSize
    }
    preferencesController.fontSizeStepper.minValue = Double(smallestFontSize)

      // Get the initial list of available fonts.
    updateAvailableFonts()

      // This is ugly! We do set `currentThemeIndexes` in `awakeFromNib()`, but the `NSArrayController` hooked up to the
      // table view in the themes preferences pane does reset `currentThemeIndexes` during its initialisation. Hence,
      // we need to set `currentThemeIndexes` here again.
    let themeIndex = (themeNames as NSArray).indexOfObject(initialThemeName)
    if themeIndex != NSNotFound { currentThemeIndexes = NSIndexSet(index: themeIndex) }

      // Tokenise the sample code by running a temporary Haskell session.
    let handler: DiagnosticsHandler = { unusedArg in return }
    let haskellSession     = HaskellSession(diagnosticsHandler: handler, interactiveWorkingDirectory: "/tmp")
    let tokens             = haskellSession.tokeniseHaskell(sampleCode, file: "PreferencesSampleCode", line: 1, column: 1)
    let highlightingTokens = map(tokens){ HighlightingToken(ghcToken: $0) }

      // Set up the the text view.
    NSScrollView.setRulerViewClass(TextGutterView)
    if let textStorage = preferencesController.sampleCodeView.layoutManager?.textStorage { // Highlighting requires the text storage delegate.
      codeStorageDelegate  = CodeStorageDelegate(textStorage: textStorage)
      textStorage.delegate = codeStorageDelegate
    }
    preferencesController.sampleCodeScrollView.hasVerticalRuler = true        // Set up the gutter.
    preferencesController.sampleCodeScrollView.rulersVisible    = true
    preferencesController.sampleCodeView.delegate               = self
    preferencesController.sampleCodeView.enableHighlighting({unusedArg in highlightingTokens})  // We always produce the same tokens.
    preferencesController.sampleCodeView.string = sampleCode
    preferencesController.sampleCodeView.highlight()

    reportThemeInformation(self,
                           fontChangeNotification:  curry{obj, font in return},
                           themeChangeNotification: curry{$0.updateThemesPane($1)})
  }

  /// Determine the list of available fixed pitch fonts and provide them by way of `availableFonts`.
  ///
  func updateAvailableFonts() {
    if let fonts = NSFontManager.sharedFontManager().availableFontNamesWithTraits(NSFontTraitMask.FixedPitchFontMask) {
      availableFonts = fonts as [String]
    }
  }

  deinit {
    NSNotificationCenter.defaultCenter().removeObserver(self)
  }

  func userDefaultsDidChange(notification: NSNotification) {
    currentFontName = NSUserDefaults.standardUserDefaults().stringForKey(kPreferenceFontName) ?? defaultFontName
    currentFontSize = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceFontSize)
  }

}


// MARK: -
// MARK: Theme status updates

extension ThemesController {

  /// Objects (e.g., code views) register to be notified of font and theme changes. The notification is automatically
  /// deregistered if the object gets deallocated. The callback will be executed right away with the initial value.
  ///
  /// We keep a strong reference to the callback functions, but *not* to the object.
  ///
  func reportThemeInformation<S: AnyObject>(object:                  S,
                                            fontChangeNotification:  S -> FontChangeNotification,
                                            themeChangeNotification: S -> ThemeChangeNotification)
  {
    fontChangeNotifications.append(WeakApply(fontChangeNotification, object))
    fontChangeNotification(object)(currentFont)
    themeChangeNotifications.append(WeakApply(themeChangeNotification, object))
    themeChangeNotification(object)(currentTheme)
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

  /// Invoke all register callbacks waiting for theme changes.
  ///
  private func notifyThemeChange(newTheme: Theme)
  {
    for notification in themeChangeNotifications {
      if let callback = notification.unbox { callback(newTheme) }
    }

      // Prune stale notification callbacks.
    themeChangeNotifications = themeChangeNotifications.filter{ $0.unbox != nil }

      // Persist current theme name in preferences.
    NSUserDefaults.standardUserDefaults().setObject(currentThemeName, forKey: kPreferenceThemeName)
  }

  /// Ensure that the colour wells in the themes pane show the colours of the given theme.
  ///
  func updateThemesPane(newTheme: Theme) {
    preferencesController?.backgroundColorWell.color = newTheme.background
    preferencesController?.invisiblesColorWell.color = newTheme.invisibles
    preferencesController?.cursorColorWell.color     = newTheme.cursor
    preferencesController?.selectionColorWell.color  = newTheme.selection
  }

  /// Action message from colour wells in the preferences pane.
  ///
  func updateColour(colorWell: NSColorWell) {
    var theme = self.currentTheme
    if colorWell == preferencesController?.categoryColorWell   {
      if let tokenKind = selectedTokenKind { theme.setTokenKind(tokenKind, colour: colorWell.color) }
    }
    if colorWell == preferencesController?.backgroundColorWell { theme.background = colorWell.color }
    if colorWell == preferencesController?.invisiblesColorWell { theme.invisibles = colorWell.color }
    if colorWell == preferencesController?.cursorColorWell     { theme.cursor     = colorWell.color }
    if colorWell == preferencesController?.selectionColorWell  { theme.selection  = colorWell.color }
    themes.updateValue(theme, forKey: currentThemeName)
  }

  func buttonPush(button: NSButton) {
    if (button.identifier == kThemeListAdd) {             // Add a duplicate of the plain theme

      addDuplicate(defaultThemes[0], newName: "New Theme")

    } else if (button.identifier == kThemeListAction) {   // Pop up the action menu

      preferencesController?.deleteThemeMenuItem.enabled = themes.count > 1
      let loc = NSPoint(x: 0, y: CGRectGetMaxY(button.bounds))
      preferencesController?.actionThemeMenu.popUpMenuPositioningItem(nil, atLocation: loc, inView: button)

    }
  }

  func actionMenuSelect(menuItem: NSMenuItem) {
    if menuItem == preferencesController?.duplicateThemeMenuItem {        // Duplicate the selected theme

      addDuplicate(currentTheme, newName: currentTheme.name + " Copy")

    } else if menuItem == preferencesController?.deleteThemeMenuItem {    // Delete the selected theme

      if themes.count == 1 { return }     // There needs to be at least one theme.
      let name = currentTheme.name
      themeNames = themeNames.filter{$0 != name}
      themes.removeValueForKey(name)

    }
  }

  func addDuplicate(var templateTheme: Theme, newName: String) {
    let name = nextName(newName, themeNames)
    templateTheme.name = name
    themes.updateValue(templateTheme, forKey: name)
    themeNames = Array(themes.keys).sorted(<=)
  }
}

extension ThemesController: NSTextViewDelegate {

  /// Notification indicating a change in the selection in the sample code view.
  ///
  func textViewDidChangeSelection(notification: NSNotification) {

      // Compute a selection choice from an array of selected tokens.
    func determineSelection(tokens: [HighlightingToken]) -> TokenSelection {
      switch tokens.count {
      case 0: return .NoSelection
      case 1: return .Selection(token: tokens[0].kind)
      default:
        let kind = tokens[0].kind
        if (tokens.filter{$0.kind != kind}).count > 0 { return .AmbiguousSelection }
        else { return .Selection(token: kind) }
      }
    }

    if let lineMap = preferencesController?.sampleCodeView.lineMap {

        // Determine the category selection and set its description.
      let selectedRange = fromNSRange(preferencesController!.sampleCodeView.selectedRange())
      let selection     = determineSelection(tokens(lineMap, inRange: selectedRange))
      preferencesController?.categoryTextField.stringValue = selection.description
      selectedTokenKind                                    = selection.highlightingTokenKind

        // Update the category colour well.
      if let tokenKind = selectedTokenKind {
        let attributes = themeToThemeDictionary(currentTheme)[tokenKind]!    // FIXME: this is not really nice
        preferencesController?.categoryColorWell.color   = attributes[NSForegroundColorAttributeName]!
        preferencesController?.categoryColorWell.enabled = true
      } else {
        preferencesController?.categoryColorWell.color   = NSColor.whiteColor()
        preferencesController?.categoryColorWell.enabled = false
      }
    }
  }
}


// MARK: -
// MARK: Sample code for the preferences pane

let sampleCode = "\n".join([ "-- Select text to edit highlighting."
                           , ""
                           , "import Data.Complex"
                           , ""
                           , "data Maybe a = Nothing | Just a"
                           , ""
                           , "{-# INLINE theAnswer #-}"
                           , "theAnswer = Just $ magnitude c ^ 2"
                           , "  where c = 6 :+ sqrt 6"
                           , ""
                           , "message = \"The answer is\" ++"
                           , "  ' ' : show theAnswer"
                           ])
