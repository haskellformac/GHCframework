//
//  AppDelegate.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

class AppDelegate: NSObject {

  // Now in PreferencesController (which is loaded at the same time as the App Delegate)
//  override class func initialize() {
//    let defaultValues = ["ExternalTextEditor": ""]    // use the default application by file extension
//    NSUserDefaults.standardUserDefaults().registerDefaults(defaultValues)
//    NSUserDefaultsController.sharedUserDefaultsController().initialValues = defaultValues
//  }

  @IBOutlet weak var preferencesController: PreferencesController!
}

extension AppDelegate: NSApplicationDelegate {

  func applicationDidFinishLaunching(notification: NSNotification) {

    // initialisation code
  }

  func orderFrontHfMAboutPanel(sender: AnyObject) {
    NSApp.orderFrontStandardAboutPanelWithOptions(["ApplicationName": "Haskell for Mac"])
  }

}
