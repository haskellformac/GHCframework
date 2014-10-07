//
//  CloudSession.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This model view class provides a session interface to the stateless cloud model in CloudcelerateKit.

import Foundation


class CloudSession {

  // MARK: -
  // MARK: Class methods (authentication etc)

  /// Get the cloud session for this HfM instance.
  ///
  /// This fails if we haven't got an API key of an authenticated account.
  ///
  class func theSession() -> ErrorOr<CloudSession> {
    let apiKey = NSData() // FIXME; grab it off the keychain
    return result(CloudSession(apiKey: apiKey))
  }

  /// Create a new MAS account with the given MAS receipt.
  ///
  class func newMASAccount(newUsername: String, storeReceipt: NSData) -> NSError? {
    return nil
  }

  /// Create a new full account for the given username and password.
  ///
  /// FIXME: Would be better to have the PW already hashed!
  class func newFullAccount(newUsername: String, newPassword: String) {

  }


  // MARK: -
  // MARK: Session setup

  /// Key to use for all requests.
  ///
  private let apiKey: NSData

  private init(apiKey: NSData) {
    self.apiKey = apiKey
  }

  func ping() -> Bool {
    return true
  }

  func run(fileURL: NSURL) {

  }
}
