//
//  SynchroScrollView.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 1/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  A Swift version of the synchronised scroll view from Apple's "Scroll View Programming Guide for Mac".

import Cocoa

class SynchroScrollView: NSScrollView {

  var synchronisedScrollView: NSScrollView?

  /// Associate a second scroll view with the current one, such that changes in the vertical position of the associated
  /// scroll view are reflected in the current one.
  //
  func startSynchronisedScrollView(scrollView: NSScrollView) {

      // In case we are already synchronising, drop that connection.
    stopSynchronising()

      // Set the new scroll view that we are tracking.
    synchronisedScrollView = scrollView
    let synchronisedContentView = synchronisedScrollView?.contentView
    if let view = synchronisedContentView {
      view.postsBoundsChangedNotifications = true
    }

      // Register for bounds changed notifications on the synchronised content view.
    NSNotificationCenter.defaultCenter().addObserver(self,
                                                     selector: "synchronisedViewContentBoundsDidChange:",
                                                     name: NSViewBoundsDidChangeNotification,
                                                     object: synchronisedContentView)

  }

  /// Break the current scroll view association.
  //
  func stopSynchronising() {
    if let view = synchronisedScrollView {

        // Remove the notification registration
      let synchronisedContentView = synchronisedScrollView?.contentView
      NSNotificationCenter.defaultCenter().removeObserver(self,
                                                          name: NSViewBoundsDidChangeNotification,
                                                          object: synchronisedContentView)

        // Drop the association
      synchronisedScrollView = nil
    }
  }

  /// Inlet for bounds change notifications of associated scroll views.
  //
  func synchronisedViewContentBoundsDidChange(notification: NSNotification) {
    if let changedContentView = notification.object as? NSClipView {

      let changedBoundsOrigin = changedContentView.documentVisibleRect.origin
      let currentBoundsOrigin = contentView.bounds.origin

        // Only adjust the scroll position if the origin actually changed.
      if currentBoundsOrigin != changedBoundsOrigin {
        let newOffset = CGPoint(x: currentBoundsOrigin.x, y: changedBoundsOrigin.y)
        contentView.scrollToPoint(newOffset)
        reflectScrolledClipView(contentView)
      }

    }
  }

}
