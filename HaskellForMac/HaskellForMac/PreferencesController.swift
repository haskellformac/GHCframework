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

  @IBOutlet weak var toolbar: NSToolbar!

  // The nib containing the associated window.
  //
  override var windowNibName : String! {
    return "Preferences"
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.hidesOnDeactivate       = false
    window?.excludedFromWindowsMenu = true
    window?.identifier              = "Preferences"
    window?.restorationClass        = PreferencesController.self

    toolbar.delegate = self
    toolbar.selectedItemIdentifier = kGeneralPreferences
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

//  func toolbarDefaultItemIdentifiers(toolbar: NSToolbar) -> [String] {
//    return [kGeneralPreferences, kTextEditingPreferences, kAccountPreferences]
//  }
//
//  func toolbarAllowedItemIdentifiers(toolbar: NSToolbar) -> [String] {
//    return [kGeneralPreferences, kTextEditingPreferences, kAccountPreferences]
//  }

  func toolbarSelectableItemIdentifiers(toolbar: NSToolbar) -> [String] {
    return [kGeneralPreferences, kTextEditingPreferences, kAccountPreferences]
  }
}

extension PreferencesController {

  @IBAction func selectPreferenceTab(sender: NSToolbarItem) {
    // nothing to be done (as we use bindings, but the toolbar item seems to need an action (and associated validation)
  }

  override func validateToolbarItem(theItem: NSToolbarItem) -> Bool {
    return true
  }
}


// MARK: -
// MARK: NSTabViewDelegate methods

extension PreferencesController: NSTabViewDelegate {

}