//
//  AppDelegate.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import Crashlytics

class AppDelegate: NSObject {

  // Now in PreferencesController (which is loaded at the same time as the App Delegate)
//  override class func initialize() {
//    let defaultValues = ["ExternalTextEditor": ""]    // use the default application by file extension
//    NSUserDefaults.standardUserDefaults().registerDefaults(defaultValues)
//    NSUserDefaultsController.sharedUserDefaultsController().initialValues = defaultValues
//  }

  @IBOutlet weak var preferencesController:   PreferencesController!
  @IBOutlet weak var documentationController: DocumentationController!
}

extension AppDelegate: NSApplicationDelegate {

  func applicationDidFinishLaunching(notification: NSNotification) {

    Crashlytics.startWithAPIKey("149e2f1c071531ba90e5d1baef11804ef2d0f2e9")
  }

  func orderFrontHfMAboutPanel(sender: AnyObject) {
    NSApp.orderFrontStandardAboutPanelWithOptions(["ApplicationName": "Haskell for Mac"])
  }

}
