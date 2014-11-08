//
//  AppDelegate.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

class AppDelegate: NSObject {

  @IBOutlet weak var preferencesController: PreferencesController!
}

extension AppDelegate: NSApplicationDelegate {

  func applicationDidFinishLaunching(notification: NSNotification) {

    // initialisation code
  }

}