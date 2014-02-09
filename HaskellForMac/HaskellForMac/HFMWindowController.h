//
//  HFMWindowController.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Controller for main windows — one window per project (aka HfM document). It is responsible for the
//  toolbar and the outline view.

#import <Cocoa/Cocoa.h>


@interface HFMWindowController : NSWindowController

#pragma mark -
#pragma mark Controlling the editor component

/// Select the editor to be displayed in this window by way of its nib file name.
//
- (void)selectEditor:(NSString *)nibName;

@end
