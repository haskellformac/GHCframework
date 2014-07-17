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

  -- standard libraries
import Control.Applicative
import Data.Time

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- GHC
import qualified GHC          as GHC
import qualified StringBuffer as GHC

  -- friends
import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Haskell side support code
-- -------------------------

-- Load a module of which the actual program code is given.
--
--FIXME: target set up is very incomplete
loadModuleText :: Session -> String -> IO String
loadModuleText session moduleText
  = do
    { utcTime <- getCurrentTime
    ; showResult <$> load session (target utcTime)
    }
  where
    showResult (Result res) = res
    showResult (Error  err) = "ERROR: " ++ err
    
    target utcTime 
      = GHC.Target
        { GHC.targetId           = GHC.TargetModule (GHC.mkModuleName "Interactive")
        , GHC.targetAllowObjCode = False
        , GHC.targetContents     = Just (GHC.stringToStringBuffer moduleText, utcTime)
        }


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface GHCInstance : NSObject

// Create a new GHC instance.
//
+ (instancetype)ghcInstanceStart;

// Load a module given as a string.
//
- (typename NSString *)loadModuleFromString:(typename NSString *)moduleText;

// Release the resources of this GHC instance. It cannot be used after this.
//
- (void)stop;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation [Typed 'start, Typed 'stop, Typed 'loadModuleText] [cunit|

@interface GHCInstance ()

// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr interpreterSession;

@end

@implementation GHCInstance

+ (instancetype)ghcInstanceStart
{
  NSLog(@"GHC instance start");
  return [[GHCInstance alloc] init];
}

- (instancetype)init
{
  self = [super init];
  if (self)
    self.interpreterSession = start();
  return self;
}

- (typename NSString *)loadModuleFromString:(typename NSString *)moduleText
{
  return loadModuleText(self.interpreterSession, moduleText);
}

- (void)stop
{
  stop(self.interpreterSession);
  NSLog(@"GHC instance stop");
}

- (void)dealloc
{
  [self stop];
  hs_free_stable_ptr(self.interpreterSession);
}

@end
|]


objc_emit

foreign export ccall "GHCInstance_initialise" objc_initialise :: IO ()
