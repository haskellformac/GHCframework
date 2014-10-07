//
//  CloudController.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  The cloud controller is in charge of the Cloudcelerate context for one particular project document. It is owned by
//  the project's window controller, much like the `ContextController` that is in charge of the editor and playground.

import Foundation


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

  /// This may only be accessed by the following getter *urgh*
  private var theSession: CloudSession?

  /// Try to obtain a cloud session.
  ///
  private var session: CloudSession? {
    get {
      // Ensure we are authenticated; if that is not possible, bail out.
      if theSession == nil {
        if let session = Optional(errorOrResult: CloudSession.theSession()) {
          theSession = session
        } else {
          if authenticationRequest(.NewAccount) {
            if let session = Optional(errorOrResult: CloudSession.theSession()) {
              theSession = session
            } else {
              // FIXME: ALERT: account set up failed (will be more complicated once we support full accounts)
            }
          }  // else user canceled the action
        }
      }
      return theSession
    }
  }

  init(project: HFMProject, authenticationRequest: AuthenticationRequest) {
    self.project = project
    self.authenticationRequest = authenticationRequest
    // FIXME: at this point we should try to authenticate with an existing API key asynchronously (if that fails,
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