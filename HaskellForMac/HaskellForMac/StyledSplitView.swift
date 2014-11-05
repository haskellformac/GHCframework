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

      // If we are already animating, cancel the animation and out the new one into the completion handler.
    if let constraint = animationConstraint {
      NSAnimationContext.runAnimationGroup(
        { context in
          context.duration               = 0.0
          constraint.animator().constant = constraint.constant
        },
        {    // Retry after cancellation
          self.animationConstraint = nil    // Don't do this earlier as it serves as a lock to starting an animation.
          self.animateSetSubview(subview, toCollapsed: collapsed, completionHandler: completion)
      })
      return
    }

    if subview.hidden == collapsed { return }           // If we are already in the target state, we are done

    if subview.hidden { subview.hidden = false }        // The view needs to be visible for the animation

    let width                  = subview.frame.size.width
    let (startWidth, endWidth) = collapsed ? (width, 1) : (1, width)

      // Remove all potentially conflicting width constraints while animating.
    let widthConstraints = subview.constraints.filter{ arrayElement in
      let thisConstraint = arrayElement as NSLayoutConstraint
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
      {
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
