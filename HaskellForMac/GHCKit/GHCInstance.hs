{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

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
import Data.Typeable
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- GHC
import qualified FastString   as GHC
import qualified GHC          as GHC
import qualified StringBuffer as GHC
import qualified SrcLoc       as GHC

  -- friends
import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "GHCSeverity.h"]


-- Haskell side support code
-- -------------------------

newtype GHCInstance = GHCInstance (ForeignPtr GHCInstance)
  deriving Typeable   -- needed for now until migrating to new TH

-- FIXME: This is ugly. We should have an 'objc_class' directive that generates this code.
objc_interface [cunit| @class GHCInstance; |]
ghcInstance :: GHCInstance -> IO GHCInstance
ghcInstance = return
objc_marshaller 'ghcInstance 'ghcInstance

-- Create a new GHC session that reports diagnostics through the provided object using the method
--
startWithHandlerObject :: GHCInstance -> IO Session
startWithHandlerObject handlerObject
  = start $ \severity srcSpan msg ->
      case GHC.srcSpanFileName_maybe srcSpan of
        Nothing     -> let spanText = GHC.showUserSpan True srcSpan 
                       in $(objc ['spanText :> ''String, 'msg :> ''String] 
                            (void [cexp| NSLog(@"%@: %@", spanText, msg) |]))
        Just fsname -> let fname = GHC.unpackFS fsname
                       in
                       $(objc ['handlerObject :> Class ''GHCInstance,
                               'fname         :> ''String,
                               'cseverity     :> ''Int,
                               'line          :> ''Word,
                               'column        :> ''Word,
                               'lines         :> ''Word,
                               'endColumn     :> ''Word,
                               'msg           :> ''String]
                         (void [cexp| [handlerObject reportWithSeverity:cseverity
                                                               filename:fname
                                                                   line:line
                                                                 column:column
                                                                  lines:lines
                                                              endColumn:endColumn
                                                                message:msg] |]))
          where
            cseverity = unsafePerformIO $ case severity of
                          GHC.SevOutput      -> $(objc [] (''Int <: [cexp| GHCSeverityOutput |]))
                          GHC.SevDump        -> $(objc [] (''Int <: [cexp| GHCSeverityDump |]))
                          GHC.SevInteractive -> $(objc [] (''Int <: [cexp| GHCSeverityInteractive |]))
                          GHC.SevInfo        -> $(objc [] (''Int <: [cexp| GHCSeverityInfo |]))
                          GHC.SevWarning     -> $(objc [] (''Int <: [cexp| GHCSeverityWarning |]))
                          GHC.SevError       -> $(objc [] (''Int <: [cexp| GHCSeverityError |]))
                          GHC.SevFatal       -> $(objc [] (''Int <: [cexp| GHCSeverityFatal |]))

            GHC.RealSrcLoc startLoc = GHC.srcSpanStart srcSpan
            GHC.RealSrcLoc endLoc   = GHC.srcSpanEnd srcSpan
            line                    = GHC.srcLocLine startLoc
            column                  = GHC.srcLocCol startLoc
            lines                   = GHC.srcLocLine endLoc - line
            endColumn               = GHC.srcLocCol endLoc

-- Load a module of which the actual program code is given.
--
--FIXME: target set up is very incomplete
loadModuleText :: Session -> String -> IO Bool
loadModuleText session moduleText
  = do
    { utcTime <- getCurrentTime
    ; showResult <$> load session (target utcTime)
    }
  where
    showResult (Result _) = True
    showResult (Error  _) = False
    
    target utcTime 
      = GHC.Target
        -- { GHC.targetId           = GHC.TargetModule (GHC.mkModuleName "Interactive")
        { GHC.targetId           = GHC.TargetFile "/Users/chak/tmp/BigPixel.hsproj/src/BigPixel.hs" Nothing
        , GHC.targetAllowObjCode = False
        , GHC.targetContents     = Just (GHC.stringToStringBuffer moduleText, utcTime)
        }

evalText :: Session -> String -> IO String
evalText session exprText 
  = showResult <$> eval session exprText
  where
    showResult (Result res) = res
    showResult (Error  err) = "ERROR: " ++ err


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

typedef void(^DiagnosticsHandler)(typename GHCSeverity  severity, 
                                  typename NSString    *fname,
                                  typename NSUInteger   line,
                                  typename NSUInteger   column,
                                  typename NSUInteger   lines,
                                  typename NSUInteger   endColumn,
                                  typename NSString    *msg);

@interface GHCInstance : NSObject

/// Initialise a new GHC instance.
///
- (instancetype)initWithDiagnosticsHandler:(DiagnosticsHandler)handler;

/// Load a module given as a string.
///
/// Any error messages and warnings are delivered via the diagnostics handler of the current object. The result
/// indicates whether loading was successful.
///
- (typename BOOL)loadModuleFromString:(typename NSString *)moduleText;

/// Evaluate the Haskell expression given as a string.
///
- (typename NSString *)evalExprFromString:(typename NSString *)exprText;

// Framework internal methods
// --

// GHC diagnostic handler.
//
- (void)reportWithSeverity:(typename NSInteger)severity
                  filename:(typename NSString *)fname
                      line:(typename NSUInteger)line 
                    column:(typename NSUInteger)column
                     lines:(typename NSUInteger)lines
                 endColumn:(typename NSUInteger)endColumn
                   message:(typename NSString *)msg;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation [Typed 'startWithHandlerObject, Typed 'stop, Typed 'loadModuleText, Typed 'evalText] [cunit|

@interface GHCInstance ()

// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr interpreterSession;

// The handler to process diagnostic messages arriving from GHC.
@property (strong) typename DiagnosticsHandler diagnosticsHandler;

@end

// Init and deinit
// --

void GHCInstance_initialise(void);

@implementation GHCInstance

+ (void)initialize
{
  GHCInstance_initialise();
}

- (instancetype)init
{
  return [self initWithDiagnosticsHandler:nil];
}

- (instancetype)initWithDiagnosticsHandler:(typename DiagnosticsHandler)handler
{
  self = [super init];
  if (self) {
    
    NSLog(@"GHC instance start");
    self.interpreterSession = startWithHandlerObject(self);
    self.diagnosticsHandler  = handler;
    
  }
  return self;
}

- (void)dealloc
{
  stop(self.interpreterSession);
  NSLog(@"GHC instance stop");
  hs_free_stable_ptr(self.interpreterSession);
}

// Public model methods
// --

- (typename BOOL)loadModuleFromString:(typename NSString *)moduleText
{
  return loadModuleText(self.interpreterSession, moduleText);
}

- (typename NSString *)evalExprFromString:(typename NSString *)exprText
{
  return evalText(self.interpreterSession, exprText);
}

// Framework internal methods
// --

- (void)reportWithSeverity:(typename NSInteger)severity 
                  filename:(typename NSString *)fname
                      line:(typename NSUInteger)line 
                    column:(typename NSUInteger)column
                     lines:(typename NSUInteger)lines
                 endColumn:(typename NSUInteger)endColumn
                   message:(typename NSString *)msg
{
  self.diagnosticsHandler(severity, fname, line, column, lines, endColumn, msg);
}

@end
|]


objc_emit

foreign export ccall "GHCInstance_initialise" objc_initialise :: IO ()
