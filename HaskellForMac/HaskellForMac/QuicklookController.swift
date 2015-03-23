//
//  QuicklookController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 13/12/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import Quartz


class QuicklookController: NSViewController {

  // The views of the quicklook view hierarchy.
  //
  @IBOutlet private weak var pathControl: NSPathControl!
  @IBOutlet private weak var contentView: NSView!

  /// Project view model item representing the previewed file.
  ///
  private let viewModelItem: ProjectItem

  /// The quicklook view that we are controlling.
  ///
  private var quicklookView: QLPreviewView!


  // MARK: -
  // MARK: Initialisation and deinitialisation

  /// Initialise the view controller by loading its NIB file and also set the associated file URL.
  ///
  init?(nibName: String!, bundle: NSBundle!, projectViewModelItem: ProjectItem) {
    viewModelItem = projectViewModelItem
    super.init(nibName: nibName, bundle: bundle)
  }

  deinit {
    quicklookView.close()
  }

  required init?(coder: NSCoder) {
    NSLog("%s: WARNING: allocating empty Quicklook controller", __FUNCTION__)
    viewModelItem = ProjectItem()
    super.init(coder: coder)
  }
  
  override func awakeFromNib() {

      // Configure path control.
    if let path = viewModelItem.filePath { self.pathControl.URL = NSURL(string: path) }

      // Set up quicklook view.
    quicklookView                       = QLPreviewView(frame: contentView.bounds)
    quicklookView.previewItem           = viewModelItem
    quicklookView.shouldCloseWithWindow = false
    quicklookView.autoresizingMask = NSAutoresizingMaskOptions.ViewWidthSizable
                                   | NSAutoresizingMaskOptions.ViewHeightSizable
    quicklookView.translatesAutoresizingMaskIntoConstraints = true
    contentView.addSubview(quicklookView)
  }
}
