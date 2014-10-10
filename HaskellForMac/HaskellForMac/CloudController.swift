//
//  CloudController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  The cloud controller is in charge of the Cloudcelerate context for one particular project document. It is owned by
//  the project's window controller, much like the `ContextController` that is in charge of the editor and playground.

import Cocoa


/* FIXME: We define this in 'HFMWindowController' for now until the later is rewritten in Swift.

/// The various flavours of authentication that the cloud controller can request of the UI.
///
enum AuthenticationFlavour {
  case NewAccount
  case AuthenticateAccount  // FIXME: this will only be used once we need to ask for login info
}
*/

/// Callback to request the UI to authenticate the Cloudcelerate user.
///
/// The result indicates whether the use approved authenticaion (== `true`) or requested to cancel (== `false`).
///
/// FIXME: this will become more complicated once we have full accounts: (1) in the case of a new account, we may get a
///        UID/PW combo back, (2) we may have to ask for a UID/PW combo to log into a full account and (3) the user
///        may ask to reset the password and only provide a UID
typealias AuthenticationRequest = AuthenticationFlavour -> Bool


  // Needs be a subclass of `NSObject` until `HFMWindowController` is rewritten in Swift (as we need to `+alloc` it).
final class CloudController : NSObject {

  /// The project in which this context is located.
  ///
  private let project: HFMProject

  /// The authentication callback.
  ///
  private let authenticationRequest: AuthenticationRequest

  /// The One Cloudcelerate session for this instance of HfM. It's lazily created.
  ///
  /// NB: This may only be accessed by the following getter *urgh*
  private var theSession: CloudSession?

  /// Try to obtain a cloud session.
  ///
  private var session: CloudSession? {
    get {
      // If we are not yet authenticated; try to sign in or sign up.
      if theSession == nil {

        // FIXME: we should not hit the keychain before the first use of Cloudcelerate (right before the initial set up;
        //   otherwise, might get a keychain dialog before they get asked for permission for the account setup
        // FIXME: when adding support for general accounts, we need to store the last used username in the app state
        let macAddress = copy_mac_address().takeRetainedValue() as NSData
        let username   = macAddress.base64EncodedStringWithOptions(nil)
        if let session = Optional(errorOrResult: CloudSession.theSession(username)) {
          theSession = session
        } else {
          if authenticationRequest(.NewAccount) {

              // Let's sign up for a MAS account.
            if let receiptURL = NSBundle.mainBundle().appStoreReceiptURL {

              let errorOrSession = CloudSession.newMASAccount(username, storeReceiptPath: receiptURL.path!)
              switch errorOrSession {

              case .Result(let session): theSession = session.unbox     // Success!

              case .Error(let error):                                   // Account creation failed; inform user
                NSLog("limited account setup failed: %@", error.description)
                let alert = NSAlert(error: error)
                alert.runModal()
              }

            } else { NSLog("limited account: missing uid") }

          }  // else user canceled signup or signup failed
        }
      }

      return theSession
    }
  }

  init(project: HFMProject, authenticationRequest: AuthenticationRequest) {
    self.project = project
    self.authenticationRequest = authenticationRequest
    // FIXME: at this point we should make a ping *with* authentication with an existing API key asynchronously (if that fails,
    //        ignore the failure until the user actually wants to use the cloud)
  }


  // MARK: -
  // MARK: Cloud authentication

  func authenticated() -> Bool {
    return false // FIXME: implement
  }

  // MARK: -
  // MARK: Cloud operations

  /// Run this project in the current configuration in the cloud.
  ///
  func run() {

    if let mySession = session {
      if let fileURL = project.fileURL {
        // FIXME: add configurations (comprising the data set, execution parameters, notification endpoints(?), etc)
        mySession.run(fileURL)
      }
    }
    // FIXME: we need to display the failure somewhere (a dialog
  }
}