//
//  CloudSession.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This model view class provides a session interface to the stateless cloud model in CloudcelerateKit.

import Foundation
import CloudcelerateKit


private let serverName = "cloudcelerate.io"

class CloudSession {

  // MARK: -
  // MARK: Class methods (authentication etc)

  /// Get the cloud session for this HfM instance.
  ///
  /// This fails if we haven't got an API key of an authenticated account.
  ///
  class func theSession(username: String) -> ErrorOr<CloudSession> {

      // Try to retrieve the API key for the given username from the keychain.
    let res: ErrorOr<CloudSession> =
      serverName.withCString{ serverNameUnsafePtr in
      username.withCString{ usernameUnsafePtr in

        var apiKeyLen: UInt32 = 0
        var apiKeyPtr: UnsafeMutablePointer<()> = nil
        let resultCode = SecKeychainFindInternetPassword(nil,
          UInt32(serverName.lengthOfBytesUsingEncoding(NSUTF8StringEncoding)), serverNameUnsafePtr,
          0, nil,
          UInt32(username.lengthOfBytesUsingEncoding(NSUTF8StringEncoding)), usernameUnsafePtr,
          0, nil,
          0,
          UInt32(kSecProtocolTypeHTTPS),
          UInt32(kSecAuthenticationTypeHTTPBasic),
          &apiKeyLen, &apiKeyPtr, nil)
        if resultCode == errSecSuccess {

          if let apiKey = NSString(bytes: apiKeyPtr, length: Int(apiKeyLen), encoding: NSUTF8StringEncoding) {
            SecKeychainItemFreeContent(nil, apiKeyPtr)

            let valid = Cloudcelerate.validateUsername(username, apiKey: apiKey)
            if valid {
              return result(CloudSession(username: username, apiKey: apiKey))
            } else {
              return error("Can't validate username")
            }
          } else {
            return error("Couldn't decode keychain result")
          }

        } else {
          return error("No API key available")
        }
      }
    }
    return res
  }

  /// Create a new MAS account with the given MAS receipt.
  ///
  class func newMASAccount(newUsername: String, storeReceiptPath: String) -> ErrorOr<CloudSession> {
    if let apiKey = Cloudcelerate.newMASAccount(newUsername, storeReceiptPath: storeReceiptPath) {

        // Store the API key as a password in the keychain for this username and the Cloudcelerate server.
      let _: () =   // this is just to keep the Swift type checker happy
        serverName.withCString{ serverNameUnsafePtr in
          newUsername.withCString{ newUsernameUnsafePtr in
            apiKey.withCString{ apiKeyUnsafePtr in
              SecKeychainAddInternetPassword(nil,
                UInt32(serverName.lengthOfBytesUsingEncoding(NSUTF8StringEncoding)), serverNameUnsafePtr,
                0, nil,
                UInt32(newUsername.lengthOfBytesUsingEncoding(NSUTF8StringEncoding)), newUsernameUnsafePtr,
                0, nil,
                0,
                UInt32(kSecProtocolTypeHTTPS),
                UInt32(kSecAuthenticationTypeHTTPBasic),
                UInt32(apiKey.lengthOfBytesUsingEncoding(NSUTF8StringEncoding)), apiKeyUnsafePtr, nil)
              return ()
            }
          }
        }
      return result(CloudSession(username: newUsername, apiKey: apiKey))

    } else {
      NSLog("failed to create a Mac Cloudcelerate account for '%@'", newUsername)
      // FIXME: need to add a suggestion on how to fix this and a reason for the failure where possible and also a link
      //   to a support website
      return error("Failed to create a Cloudcelerate account.")
    }
  }

  /// Create a new full account for the given username and password.
  ///
  /// FIXME: Would be better to have the PW already hashed!
  class func newFullAccount(newUsername: String, newPassword: String) -> ErrorOr<CloudSession> {
    NSLog("%s: full account creation is not implemented yet", __FUNCTION__)
    return error("Full account creation is not implemented yet.")
  }


  // MARK: -
  // MARK: Session setup

  /// Username for all requests.
  ///
  private let username: String

  /// Key to use for all requests.
  ///
  private let apiKey: String

  private init(username: String, apiKey: String) {
    self.username = username
    self.apiKey   = apiKey
  }


  // MARK: -
  // MARK: Cloudcelerate API calls

  /// Check whether the Cloudcelerate server is alive (without authentication).
  ///
  func ping() -> Bool {
    return Cloudcelerate.validateUsername(nil, apiKey: "")
  }

  /// Upload the given project and issue a job running the program on an empty dataset.
  ///
  func run(fileURL: NSURL) -> ErrorOr<()> {
    let outcome = Cloudcelerate.uploadProgramFor(username, apiKey: apiKey, fileURL: fileURL)
                  ?? Cloudcelerate.runJobFor(username, apiKey: apiKey, programName: fileURL.lastPathComponent, dataName: nil)
    return ErrorOr(optionalError: outcome, result: ())
  }
}
