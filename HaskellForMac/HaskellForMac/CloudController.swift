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
  /// This always fails if Cloud services are not enabled.
  ///
  private var session: CloudSession? {
    get {
      let userDefaults = NSUserDefaults.standardUserDefaults()
      if !userDefaults.boolForKey(kPreferenceEnableCloud) { return nil }      // Cloud disabled => refuse session

        // If we are not yet authenticated; try to sign in or sign up.
      if theSession == nil {

          // If there is no account name in preferences, we need to sign up for an account first.
          //
          // NB: It's important to check the defaults before hitting the keychain as we don't want to risk a keychain
          //     permission alert before the sign up permission dialog.
        if let username = userDefaults.stringForKey(kPreferenceUsername) {   // sign in
          switch CloudSession.theSession(username) {

          case .Result(let session): theSession = session.unbox

          case .Error(let error):
            NSLog("can't authenticate: %@", error.description)
            let alert = NSAlert(error: error)
            alert.runModal()
          }
        } else {                                                             // sign up

            // The username for a Mac account is the GUID.
          let macAddress  = copy_mac_address().takeRetainedValue() as NSData
          let newUsername = macAddress.base64EncodedStringWithOptions(nil)
          if let (finalUsername, session) = signup(newUsername) {

            userDefaults.setObject(finalUsername, forKey: kPreferenceUsername)
            theSession = session
          }
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
  // MARK: Cloud sign up

  private func signup(username: String) -> (finalUsername: String, session: CloudSession)? {

    if authenticationRequest(.NewAccount) {

        // Let's sign up for a MAS account.
      if let receiptURL = NSBundle.mainBundle().appStoreReceiptURL {

        let errorOrSession = CloudSession.newMASAccount(username, storeReceiptPath: receiptURL.path!)
        switch errorOrSession {

        case .Result(let session): return (username, session.unbox)     // Success!
                                          // we return the same username — for a full acount setup, we will return a different name

        case .Error(let error):                                         // Account creation failed; inform user
          NSLog("limited account setup failed: %@", error.description)
          NSAlert(error: error).runModal()
          return nil
        }

      } else { NSLog("limited account: missing uid"); return nil }
    } else { return nil }   // user rejected sign up
  }

  /// Have we signed up for a Mac account already?
  ///
  /// NB: This doesn't hit the network (i.e., can be used to validate interface items etc).
  ///
  /// FIXME: the result needs to be tertiary: (1) no account, (2) Mac account, or (3) full account.
  func accountStatus() -> Bool {
    let userDefaults = NSUserDefaults.standardUserDefaults()
    return userDefaults.stringForKey(kPreferenceUsername) != nil
  }

  
  // MARK: -
  // MARK: Cloud operations

  /// Ping cloudcelerate after ensuring we have a valid session (i.e., account is set up).
  ///
  func ping() {
    if let mySession = session { mySession.ping() }
  }

  /// Run this project in the current configuration in the cloud.
  ///
  func run() {

    if let mySession = session {
      if let fileURL = project.fileURL {
        // FIXME: add configurations (comprising the data set, execution parameters, notification endpoints(?), etc)

        switch mySession.run(fileURL) {

        case .Error(let error):
          NSAlert(error: error).runModal()
        default: ()
        }
      }
    }
  }
}