//
//  PlaygroundController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/07/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa

class PlaygroundController: NSViewController {

  @IBOutlet weak var splitView: NSSplitView!
  @IBOutlet weak var codeScrollView: SynchroScrollView!
  @IBOutlet weak var resultScrollView: NSScrollView!
  @IBOutlet      var codeTextView: NSTextView!
  @IBOutlet      var resultTextView: NSTextView!

  override func viewDidLoad() {
    super.viewDidLoad()

      // Synchronise the scroll views.
    codeScrollView.setSynchronisedScrollView(resultScrollView)
  }

  deinit {
    codeScrollView.stopSynchronising()
  }
}
