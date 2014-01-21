//
//  HFMProject.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProject.h"


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

- (NSString *)windowNibName
{
  return @"ProjectWindow";
}


@end
