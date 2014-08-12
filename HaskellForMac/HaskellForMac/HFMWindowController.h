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
#import "Haskell-Swift.h"


@interface HFMWindowController : NSWindowController <NSOutlineViewDelegate, NSSplitViewDelegate>


#pragma mark -
#pragma mark Notifications

/// The data source of the outline view changed. Bring the view up to date.
///
- (void)refreshOutlineView;

@end
