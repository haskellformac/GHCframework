{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : GHCInstance
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
--
-- This module implements the Objective-C to Haskell bridge for managing GHC instances.

module GHCInstance () where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
-- import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface GHCInstance : NSObject

// Create a new GHC instance.
//
+ (instancetype)ghcInstanceStart;

// Release the resources of this GHC instance. It cannot be used after this.
//
- (void)stop;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation [] [cunit|

@implementation GHCInstance

+ (instancetype)ghcInstanceStart
{
  NSLog(@"GHC instance start");
  return [[GHCInstance alloc] init];
}

- (void)stop
{
  NSLog(@"GHC instance stop");
}

@end
|]


objc_emit

foreign export ccall "GHCInstance_initialise" objc_initialise :: IO ()
