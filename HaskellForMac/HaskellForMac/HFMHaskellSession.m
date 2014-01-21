//
//  HFMGHC.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMHaskellSession.h"


@implementation HFMHaskellSession

#pragma mark -
#pragma Initialisation

+ (instancetype)haskellSessionStart
{
  return [[HFMHaskellSession alloc] init];
}

- (instancetype)init
{
  self = [super init];
  return self;
}

@end
