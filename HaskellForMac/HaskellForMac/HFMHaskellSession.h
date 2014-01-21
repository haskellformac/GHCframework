//
//  HFMGHCSession.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Each instance of this class represents a GHC session.

#import <Foundation/Foundation.h>


@interface HFMHaskellSession : NSObject

// Start a new GHC session.
//
+ (instancetype)haskellSessionStart;

@end
