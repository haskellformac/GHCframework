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


@interface HFMWindowController : NSWindowController <NSOutlineViewDelegate, NSSplitViewDelegate>

#pragma mark -
#pragma mark Controlling the editor component

/// Select the editor appropriate to editing files with the given extension.
//
// If no suitable editor is available, remove the current editor view (if any).
//
- (void)selectEditor:(NSString *)fileExtension;

@end
