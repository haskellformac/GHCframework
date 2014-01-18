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

- (instancetype)initWithCopy:(HFMProject *)project
{
  self = [super init];
  if (self) {
    _name = project.name;
  }
  return self;
}


#pragma mark -
#pragma mark NSCopying protocol

- (instancetype)copyWithZone:(NSZone *)zone
{
  return [[HFMProject allocWithZone:zone] initWithCopy:self];
}

@end
