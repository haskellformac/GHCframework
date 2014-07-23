//
//  HFMHaskellSession.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "CabalKit/CabalKit.h"
#import "GHCKit/GHCKit.h"
#import "HFMHaskellSession.h"


@interface HFMHaskellSession ()

@property (readonly, nonatomic) GHCInstance *ghcInstance;

@end


@implementation HFMHaskellSession


#pragma mark -
#pragma mark Session startup

+ (instancetype)haskellSessionStart
{
  return [[HFMHaskellSession alloc] init];
}

- (instancetype)init
{
  self = [super init];
  if (self) {

    _ghcInstance = [GHCInstance ghcInstanceStart];

  }
  return self;
}


#pragma mark -
#pragma mark Session shutdown

- (void)dealloc
{
  [self.ghcInstance stop];
}


#pragma mark -
#pragma mark Code loading

- (NSString *)loadModuleFromString:(NSString *)moduleText
{
  return [self.ghcInstance loadModuleFromString:moduleText];
}


#pragma mark -
#pragma mark Code execution

- (NSString *)evalExprFromString:(NSString *)exprText
{
  return [self.ghcInstance evalExprFromString:exprText];
}

@end
