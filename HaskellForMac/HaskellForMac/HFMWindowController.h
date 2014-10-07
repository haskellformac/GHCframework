//
//  HFMWindowController.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Controller for main windows — one window per project (aka HfM document). It is responsible for the
//  toolbar, the outline view, and the split views. Hence, it serves as their delegate.

#import <Cocoa/Cocoa.h>


  // FIXME: This ought to be defined in 'CloudController' (once this class is rewritten in Swift)
typedef NS_ENUM(NSInteger, AuthenticationFlavour) {
  AuthenticationFlavourNewAccount,
  AuthenticationFlavourAuthenticateAccount
};


@interface HFMWindowController : NSWindowController <NSOutlineViewDelegate,
                                                     NSSplitViewDelegate,
                                                     NSUserInterfaceValidations,
                                                     NSTextFieldDelegate>

#pragma mark -
#pragma mark Notifications

/// The data source of the outline view changed. Bring the view up to date.
///
- (void)refreshOutlineView;


#pragma mark -
#pragma mark Menu actions

/// Initiate the asynchronous execution of the project in the cloud.
///
- (void)runProjectInCloud:(id)sender;

@end
