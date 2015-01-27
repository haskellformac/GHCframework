//
//  ValidatingToolbarItem.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 27/01/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//
//  This is an `NSToolbarItem` variant that helps validating toolbar items containing views. To this end, a validator
//  can be registered for such toolbar items.

import Cocoa


class ValidatingToolbarItem: NSToolbarItem {

  var validator: ValidatingToolbarItem -> Void = { str in }

  override func validate() {
    validator(self)
  }
}
