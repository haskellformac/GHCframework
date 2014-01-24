{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : HFMGHCInstance
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
--
-- This module implements the Objective-C to Haskell bridge for managing GHC instances.

module HFMGHCInstance () where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
-- import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface HFMGHCInstance : NSObject

// Create a new GHC instance.
//
+ (typename instancetype)ghcInstanceStart;

// Release the resources of this GHC instance. It cannot be used after this.
//
- (void)stop;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation [] [cunit|

@implementation HFMGHCInstance

+ (typename instancetype)ghcInstanceStart
{
  NSLog(@"GHC instance start");
  return [[HFMGHCInstance alloc] init];
}

- (void)stop
{
  NSLog(@"GHC instance stop");
}

@end
|]


objc_emit

foreign export ccall "HFMGHCInstance_initialise" objc_initialise :: IO ()
