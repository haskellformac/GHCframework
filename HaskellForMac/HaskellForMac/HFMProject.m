//
//  HFMProject.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProject.h"
#import "HFMWindowController.h"


@implementation HFMProject


#pragma mark -
#pragma mark Initialisation

- (instancetype)init
{
  self = [super init];
  return self;
}


#pragma mark -
#pragma mark NSDocument methods

  // FIXME: reading and writing document data

- (void)makeWindowControllers
{
  [self addWindowController:[[HFMWindowController alloc] init]];
}

  // FIXME: post-nib-loading code

  // Opting into autosaving (at least for the .hsproj file, but really also for code files)


@end
