//
//  DocumentationController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 26/01/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import WebKit


let documentationPath = "Contents/Frameworks/GHC.framework/Versions/Current/usr/share/doc/ghc/html/libraries/index.html"
let documentationRoot = NSBundle.mainBundle().bundlePath.stringByAppendingPathComponent(documentationPath)
let haskellPortal     = NSURL(string: "https://haskell.org/")!

class DocumentationController: NSWindowController {

  @IBOutlet private weak var webView:            WebView!
  @IBOutlet private weak var backToolbarItem:    ValidatingToolbarItem!
  @IBOutlet private weak var forwardToolbarItem: ValidatingToolbarItem!

  // The nib containing the associated window.
  //
  override var windowNibName : String! {
    return "Documentation"
  }

  // Reopen the preferences window when the app's persistent state is restored.
  //
  class func restoreWindowWithIdentifier(identifier: String,
    state: NSCoder,
    completionHandler: (NSWindow, NSError!) -> Void) -> Bool
  {
    completionHandler(((NSApp as! NSApplication).delegate as! AppDelegate).documentationController.window!, nil)
    return true
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.identifier       = "Documentation"
    window?.restorationClass = DocumentationController.self
    window?.title            = "Haskell Library Documentation"

    backToolbarItem.validator    = toolbarItemValidator()
    forwardToolbarItem.validator = toolbarItemValidator()

    webView.mainFrameURL = documentationRoot
  }
}

extension DocumentationController {

    // NB: We need the extra () argument to work around a swiftc bug.
  func toolbarItemValidator()(item: ValidatingToolbarItem) {
    if item == backToolbarItem    { item.enabled = webView.canGoBack }
    if item == forwardToolbarItem { item.enabled = webView.canGoForward }
  }

  @IBAction func gotoHaskellPortal(sender: NSMenuItem) {
    NSWorkspace.sharedWorkspace().openURL(haskellPortal)
  }

  @IBAction func shareToSafari(sender: NSButton) {
    if let url = NSURL(string: webView.mainFrameURL) {
      NSWorkspace.sharedWorkspace().openURL(url)
    }
  }
}
