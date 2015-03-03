//
//  StyledSplitView.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 3/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Split view that supports custom divider colours and animation of subview collapsing and uncollapsing.
//
//  A collapsed subview is hidden (that's how AppKit handled it, too) with its old size still intact. Animation is by
//  temporary constraints reconfiguring the view over the course of the animation. If another animation is requested,
//  while one is already in progress, we cancel the first one before issuing the second. (This is to avoid conflicts.)
//  Only one subview is being animated at any one time.
//
//  We don't animate on Mavericks for now as it leads to UI glitches in its current form. The pre-Yosemite code also
//  assumes, in its size calculation, that we do not hide the divide of a collapsed subview. Moreover, it assumes we are
//  having a horizontal split view. (All this could be easily generalised.)

import Cocoa


@IBDesignable
class StyledSplitView: NSSplitView {

  // MARK: -
  // MARK: Customise the divider colour

  @IBInspectable
  var customDividerColor: NSColor = NSColor.redColor()

  override var dividerColor: NSColor {
    get {
      return customDividerColor
    }
  }

  // MARK: -
  // MARK: Animation of collapsing and uncollapsing subviews.

  @IBInspectable
  var animationDuration: NSTimeInterval = 0.2

  // If an animation is underway, the constraint guiding the animation is here.
  //
  private var animationConstraint: NSLayoutConstraint? = nil

  /// Change the state of the given subview as specified.
  ///
  /// In any case, cancel any currently executing animation, even if we are already in the target state.
  ///
  func animateSetSubview(subview: NSView, toCollapsed collapsed: Bool, completionHandler completion: (() -> ())?) {

      // If we are already animating, cancel the animation and put the new one into the completion handler.
    if let constraint = animationConstraint {
      NSAnimationContext.runAnimationGroup(
        { context in
          context.duration               = 0.0
          constraint.animator().constant = constraint.constant
        },
        completionHandler: {    // Retry after cancellation
          self.animationConstraint = nil    // Don't do this earlier as it serves as a lock to starting an animation.
          self.animateSetSubview(subview, toCollapsed: collapsed, completionHandler: completion)
      })
      return
    }

    if subview.hidden == collapsed { return }           // If we are already in the target state, we are done.

      // If we are not at least on OS X 10.10, just hidding a subview is not sufficient to collapse it. Hence, we
      // need to do some extra work and cut out the animation.
    if !isOperatingSystemAtLeastVersion10_10() {
      legacyToggleCollapsedState(subview)
      completion?()
      return
    }

    if subview.hidden { subview.hidden = false }        // The view needs to be visible for the animation

    let width                  = subview.frame.size.width
    let (startWidth, endWidth) = collapsed ? (width, 1) : (1, width)

      // Remove all potentially conflicting width constraints while animating.
    let widthConstraints = subview.constraints.filter{ arrayElement in
      let thisConstraint = arrayElement as! NSLayoutConstraint
      return thisConstraint.firstAttribute == .Width && thisConstraint.firstItem === subview
             && thisConstraint.secondAttribute == .NotAnAttribute
    }
    subview.removeConstraints(widthConstraints)

      // Create and add the fixed width constraint that controls the animation.
    let constraint = NSLayoutConstraint(item: subview, attribute: .Width, relatedBy: .Equal,
                                        toItem: nil, attribute: .NotAnAttribute,
                                        multiplier: 1, constant: startWidth)
    subview.addConstraint(constraint)
    animationConstraint = constraint

    NSAnimationContext.runAnimationGroup(
      { context in
        context.duration               = self.animationDuration
        constraint.animator().constant = endWidth;
      },
      completionHandler: {
        self.animationConstraint = nil

          // Whatever happened, we need to set the final hidden state and restore the view's width and constraints.
        subview.hidden           = collapsed
        subview.frame.size.width = width            // Even if collapsed, the hidden frame needs the original width
        subview.removeConstraint(constraint)
        subview.addConstraints(widthConstraints)    // Restore temporarily removed width constraints.

          // Run the completion handler.
        completion?()
    })
  }
}

// The following code is only exercised on systems before OS X 10.10, where simply hiding a subview is not sufficient
// to collapse it.
//
// NB: This code is *not* safe on 10.10. It may lead to autolayout exceptions in certain situations.
//
extension StyledSplitView {

  func legacyToggleCollapsedState(subview: NSView) {

    let subviewIndex = find(subviews as! [NSView], subview)!
    let isCollapsed  = isSubviewCollapsed(subview)
    let isRightmost  = subviewIndex == subviews.endIndex - 1
    if subviewIndex == NSNotFound { return } // PANIC

      // We always move the divide to the right of the to-be-collapsed view to the left; unless it is the rightmost view,
      // then we move the divider to the left over to the right.
    if isCollapsed {
      if !isRightmost {
        setPosition(CGRectGetMaxX((subviews[subviewIndex] as! NSView).frame), ofDividerAtIndex: subviewIndex)
      } else {
        setPosition(CGRectGetMinX((subviews[subviewIndex] as! NSView).frame), ofDividerAtIndex: subviewIndex - 1)
      }
    } else {
      if !isRightmost {
        setPosition(CGRectGetMinX((subviews[subviewIndex] as! NSView).frame), ofDividerAtIndex: subviewIndex)
      } else {
        setPosition(CGRectGetMaxX((subviews[subviewIndex] as! NSView).frame), ofDividerAtIndex: subviewIndex - 1)
      }
    }
  }
}