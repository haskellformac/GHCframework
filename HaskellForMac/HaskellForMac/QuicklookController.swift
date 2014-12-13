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
  private let viewModelItem: HFMProjectViewModelItem


  //MARK: -
  //MARK: Initialisation and deinitialisation

  /// Initialise the view controller by loading its NIB file and also set the associated file URL.
  ///
  init?(nibName: String!, bundle: NSBundle!, projectViewModelItem: HFMProjectViewModelItem!) {
    viewModelItem = projectViewModelItem

    super.init(nibName: nibName, bundle: bundle)
  }

  required init?(coder: NSCoder) {
    NSLog("%s: WARNING: allocating empty project view model item", __FUNCTION__)
    viewModelItem = HFMProjectViewModelItem()
    super.init(coder: coder)
  }
  
  override func awakeFromNib() {

      // Set up the path control.
    self.pathControl.URL = NSURL(string: viewModelItem.filePath())
  }

  override func viewDidLayout() {
    let quicklookView = QLPreviewView(frame: contentView.bounds)
    quicklookView.previewItem = viewModelItem
    contentView.addSubview(quicklookView)
    quicklookView.needsDisplay = true

  }
}
