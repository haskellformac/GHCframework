//
//  PreferencesController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa


// IB identifiers of the various preferences views.
//
let kGeneralPreferences     = "GeneralPreferences"
let kTextEditingPreferences = "TextEditingPreferences"
let kAccountPreferences     = "AccountPreferences"

class PreferencesController: NSWindowController {

  override init() {
    super.init()
//    PreferencesController(windowNibName: "Preferences")
    super.init(windowNibName: "Preferences")
  }

  override init(window: NSWindow!) {
    super.init(window: window)
  }

  required init?(coder: NSCoder) {
      super.init(coder: coder)
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.hidesOnDeactivate       = false
    window?.excludedFromWindowsMenu = true
    window?.identifier              = "Preferences"
    window?.restorationClass        = PreferencesController.self
  }

  // Reopen the preferences window when the app's persistent state is restored.
  //
  class func restoreWindowWithIdentifier(identifier: String,
                                         state: NSCoder,
                                         completionHandler: (NSWindow, NSError!) -> Void) -> Bool
  {
    completionHandler(((NSApp as NSApplication).delegate as AppDelegate).preferencesController.window!, nil)
    return true
  }

  // MARK: -
  // MARK: Window delegate
  //
  // NB: We set this controller as the window's delegate in IB.

    // We do this to catch the case where the user enters a value into one of the text fields but closes the window
    // without hitting enter or tab.
    //
  func windowShouldClose(window: NSWindow) -> Bool {
    return window.makeFirstResponder(nil)   // validate editing
  }
}


// MARK: -
// MARK: NSToolbarDelegate methods

extension PreferencesController: NSToolbarDelegate {

  func toolbarSelectableItemIdentifiers(_toolbar: NSToolbar) -> [String] {
    return [kGeneralPreferences, kTextEditingPreferences, kAccountPreferences]
  }
}


// MARK: -
// MARK: NSTabViewDelegate methods

extension PreferencesController: NSTabViewDelegate {

}