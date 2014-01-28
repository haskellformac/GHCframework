{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : CBLPackage
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
--
-- This module implements the Objective-C to Haskell bridge for Cabal packages.

module CBLPackage () where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
-- import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface CBLPackage : NSObject

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation [] [cunit|

@implementation CBLPackage


@end
|]


objc_emit

foreign export ccall "CBLPackage_initialise" objc_initialise :: IO ()
