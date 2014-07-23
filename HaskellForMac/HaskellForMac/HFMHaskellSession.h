//
//  HFMGHCSession.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Each instance of this class represents a GHC session. This class is a view model interfacing the Haskell-side model.

#import <Foundation/Foundation.h>


@interface HFMHaskellSession : NSObject

// Start a new GHC session.
//
+ (instancetype)haskellSessionStart;


#pragma mark -
#pragma mark Code loading

// Load a single module and make it the current evaluation context.
//
- (NSString *)loadModuleFromString:(NSString *)moduleText;


#pragma mark -
#pragma mark Code execution

// Evaluate an expression in the current evaluation context.
//
- (NSString *)evalExprFromString:(NSString *)exprText;

@end
