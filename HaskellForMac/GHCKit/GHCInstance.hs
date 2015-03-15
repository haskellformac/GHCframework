{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving #-}

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
import Control.Monad        hiding (void)
import Data.List
import Data.Time
import Data.Typeable
import Data.Word
import System.IO.Unsafe     (unsafePerformIO)

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- GHC
import qualified FastString   as GHC
import qualified GHC          as GHC
import qualified Lexer        as GHC
import qualified Outputable   as GHC
import qualified StringBuffer as GHC
import qualified SrcLoc       as GHC

  -- friends
import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "GHC/HsFFI.h", "GHCKit/GHCSeverity.h", "GHCKit/GHCToken.h"]


-- Marshalling support
-- -------------------
-- FIXME: this should go into a general library — we have the same code in 'CBLPackage.hs' :(

newtype NSString = NSString (ForeignPtr NSString)
  deriving Typeable   -- needed for now until migrating to new TH

newtype NSMutableArray e = NSMutableArray (ForeignPtr (NSMutableArray e))
  deriving Typeable   -- needed for now until migrating to new TH
newtype NSArray        e = NSArray        (ForeignPtr (NSArray        e))
  deriving Typeable   -- needed for now until migrating to new TH

unsafeFreezeNSMutableArray :: NSMutableArray e -> NSArray e
unsafeFreezeNSMutableArray (NSMutableArray fptr) = NSArray $ castForeignPtr fptr

objc_typecheck

stringToNSString :: String -> IO NSString
stringToNSString str
  = $(objc ['str :> ''String] $ Class ''NSString <: [cexp| str |])

listOfStringToNSArray :: [String] -> IO (NSArray NSString)
listOfStringToNSArray strs
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray NSString|] <: [cexp| [NSMutableArray arrayWithCapacity:50] |])
    ; mapM_ (addElement marr) strs
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr str
      = $(objc ['marr :> Class [t|NSMutableArray NSString|], 'str :> ''String] $ void [cexp| [marr addObject:str] |])

nsArrayTolistOfString :: NSArray NSString -> IO [String]
nsArrayTolistOfString arr
  = do
    { n <- $(objc ['arr :> Class [t|NSArray NSString|]] $ ''Int <: [cexp| (int)arr.count |])
    ; sequence [ $(objc ['arr :> Class [t|NSArray NSString|], 'i :> ''Int] $ 
                     ''String <: [cexp| arr[(typename NSUInteger)i] |]) 
               | i <- [0..n-1]]
    }

objc_marshaller 'listOfStringToNSArray 'nsArrayTolistOfString


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
-- 'reportWithSeverity:filename:line:column:lines:endColumn:message:'.
--
-- The file path (third argument) is the working directory for interactive Haskell execution.
--
startWithHandlerObject :: GHCInstance -> Int -> Maybe String{-FilePath-} -> IO Session
startWithHandlerObject handlerObject logLevel cwd
  = do
    { 
    ; ghcBundlePath <-
        $(objc [] $ ''String <: 
           [cexp| [[NSBundle mainBundle].bundlePath stringByAppendingPathComponent:@"Contents/Frameworks"] |])
    ; when (logLevel > 0) $
        $(objc ['ghcBundlePath :> ''String] $ void [cexp| NSLog(@"bundle frameworks path: %@", ghcBundlePath) |])
    ; start ghcBundlePath (reportDiagnostics handlerObject) logLevel logString cwd
    }
  where
    logString = \str -> $(objc ['str :> ''String] $ void [cexp| NSLog(@"%@", str) |])

reportDiagnostics :: GHCInstance -> GHC.Severity -> GHC.SrcSpan -> String -> IO ()
reportDiagnostics handlerObject severity srcSpan msg
  = case GHC.srcSpanFileName_maybe srcSpan of
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
          lines                   = GHC.srcLocLine endLoc - line + 1
          endColumn               = GHC.srcLocCol endLoc

deriving instance (Typeable GHC.Token)

objc_typecheck

tokenListToNSArray :: [GHC.Located GHC.Token] -> IO (NSArray (GHC.Located GHC.Token))
tokenListToNSArray tokens
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray (GHC.Located GHC.Token)|] <: [cexp| [NSMutableArray arrayWithCapacity:50] |])
      ; mapM_ (addElement marr) tokens
      ; return $ unsafeFreezeNSMutableArray marr
      }
    where
      addElement marr locatedToken
        = $(objc ['marr      :> Class [t|NSMutableArray (GHC.Located GHC.Token)|],
                  'tok       :> ''Int,
                  'line      :> ''Word,
                  'column    :> ''Word,
                  'lines     :> ''Word,
                  'endColumn :> ''Word] $ 
              void [cexp| [marr addObject:[[GHCLocatedToken alloc] initWithToken:tok
                                                                            line:line 
                                                                          column:column
                                                                           lines:lines
                                                                       endColumn:endColumn]] |])
        where
          srcSpan = GHC.getLoc locatedToken
          token   = GHC.unLoc  locatedToken
          
          tok = unsafePerformIO $ case token of
                  GHC.ITas             -> $(objc [] (''Int <: [cexp| GHCTokenAs |]))
                  GHC.ITcase           -> $(objc [] (''Int <: [cexp| GHCTokenCase |]))
                  GHC.ITclass          -> $(objc [] (''Int <: [cexp| GHCTokenClass |]))
                  GHC.ITdata           -> $(objc [] (''Int <: [cexp| GHCTokenData |]))
                  GHC.ITdefault        -> $(objc [] (''Int <: [cexp| GHCTokenDefault |]))
                  GHC.ITderiving       -> $(objc [] (''Int <: [cexp| GHCTokenDeriving |]))
                  GHC.ITdo             -> $(objc [] (''Int <: [cexp| GHCTokenDo |]))
                  GHC.ITelse           -> $(objc [] (''Int <: [cexp| GHCTokenElse |]))
                  GHC.IThiding         -> $(objc [] (''Int <: [cexp| GHCTokenHiding |]))
                  GHC.ITif             -> $(objc [] (''Int <: [cexp| GHCTokenIf |]))
                  GHC.ITimport         -> $(objc [] (''Int <: [cexp| GHCTokenImport |]))
                  GHC.ITin             -> $(objc [] (''Int <: [cexp| GHCTokenIn |]))
                  GHC.ITinfix          -> $(objc [] (''Int <: [cexp| GHCTokenInfix |]))
                  GHC.ITinfixl         -> $(objc [] (''Int <: [cexp| GHCTokenInfixl |]))
                  GHC.ITinfixr         -> $(objc [] (''Int <: [cexp| GHCTokenInfixr |]))
                  GHC.ITinstance       -> $(objc [] (''Int <: [cexp| GHCTokenInstance |]))
                  GHC.ITlet            -> $(objc [] (''Int <: [cexp| GHCTokenLet |]))
                  GHC.ITmodule         -> $(objc [] (''Int <: [cexp| GHCTokenModule |]))
                  GHC.ITnewtype        -> $(objc [] (''Int <: [cexp| GHCTokenNewtype |]))
                  GHC.ITof             -> $(objc [] (''Int <: [cexp| GHCTokenOf |]))
                  GHC.ITqualified      -> $(objc [] (''Int <: [cexp| GHCTokenQualified |]))
                  GHC.ITthen           -> $(objc [] (''Int <: [cexp| GHCTokenThen |]))
                  GHC.ITtype           -> $(objc [] (''Int <: [cexp| GHCTokenType |]))
                  GHC.ITwhere          -> $(objc [] (''Int <: [cexp| GHCTokenWhere |]))
                  GHC.ITforall         -> $(objc [] (''Int <: [cexp| GHCTokenForall |]))
                  GHC.ITforeign        -> $(objc [] (''Int <: [cexp| GHCTokenForeign |]))
                  GHC.ITexport         -> $(objc [] (''Int <: [cexp| GHCTokenExport |]))
                  GHC.ITlabel          -> $(objc [] (''Int <: [cexp| GHCTokenLabel |]))
                  GHC.ITdynamic        -> $(objc [] (''Int <: [cexp| GHCTokenDynamic |]))
                  GHC.ITsafe           -> $(objc [] (''Int <: [cexp| GHCTokenSafe |]))
                  GHC.ITinterruptible  -> $(objc [] (''Int <: [cexp| GHCTokenInterruptible |]))
                  GHC.ITunsafe         -> $(objc [] (''Int <: [cexp| GHCTokenUnsafe |]))
                  GHC.ITstdcallconv    -> $(objc [] (''Int <: [cexp| GHCTokenStdcallconv |]))
                  GHC.ITccallconv      -> $(objc [] (''Int <: [cexp| GHCTokenCcallconv |]))
                  GHC.ITcapiconv       -> $(objc [] (''Int <: [cexp| GHCTokenCapiconv |]))
                  GHC.ITprimcallconv   -> $(objc [] (''Int <: [cexp| GHCTokenPrimcallconv |]))
                  GHC.ITjavascriptcallconv
                                       -> $(objc [] (''Int <: [cexp| GHCTokenJavascriptcallconv |]))
                  GHC.ITmdo            -> $(objc [] (''Int <: [cexp| GHCTokenMdo |]))
                  GHC.ITfamily         -> $(objc [] (''Int <: [cexp| GHCTokenFamily |]))
                  GHC.ITrole           -> $(objc [] (''Int <: [cexp| GHCTokenRole |]))
                  GHC.ITgroup          -> $(objc [] (''Int <: [cexp| GHCTokenGroup |]))
                  GHC.ITby             -> $(objc [] (''Int <: [cexp| GHCTokenBy |]))
                  GHC.ITusing          -> $(objc [] (''Int <: [cexp| GHCTokenUsing |]))
                  GHC.ITpattern        -> $(objc [] (''Int <: [cexp| GHCTokenPattern |]))
                  GHC.ITctype          -> $(objc [] (''Int <: [cexp| GHCTokenCtype |]))
                  --
                  GHC.ITdotdot         -> $(objc [] (''Int <: [cexp| GHCTokenDotdot |]))
                  GHC.ITcolon          -> $(objc [] (''Int <: [cexp| GHCTokenColon |]))
                  GHC.ITdcolon         -> $(objc [] (''Int <: [cexp| GHCTokenDcolon |]))
                  GHC.ITequal          -> $(objc [] (''Int <: [cexp| GHCTokenEqual |]))
                  GHC.ITlam            -> $(objc [] (''Int <: [cexp| GHCTokenLam |]))
                  GHC.ITlcase          -> $(objc [] (''Int <: [cexp| GHCTokenLcase |]))
                  GHC.ITvbar           -> $(objc [] (''Int <: [cexp| GHCTokenVbar |]))
                  GHC.ITlarrow         -> $(objc [] (''Int <: [cexp| GHCTokenLarrow |]))
                  GHC.ITrarrow         -> $(objc [] (''Int <: [cexp| GHCTokenRarrow |]))
                  GHC.ITat             -> $(objc [] (''Int <: [cexp| GHCTokenAt |]))
                  GHC.ITtilde          -> $(objc [] (''Int <: [cexp| GHCTokenTilde |]))
                  GHC.ITtildehsh       -> $(objc [] (''Int <: [cexp| GHCTokenTildehsh |]))
                  GHC.ITdarrow         -> $(objc [] (''Int <: [cexp| GHCTokenDarrow |]))
                  GHC.ITminus          -> $(objc [] (''Int <: [cexp| GHCTokenMinus |]))
                  GHC.ITbang           -> $(objc [] (''Int <: [cexp| GHCTokenBang |]))
                  GHC.ITstar           -> $(objc [] (''Int <: [cexp| GHCTokenStar |]))
                  GHC.ITdot            -> $(objc [] (''Int <: [cexp| GHCTokenDot |]))
                  GHC.ITbiglam         -> $(objc [] (''Int <: [cexp| GHCTokenBiglam |]))
                  GHC.ITocurly         -> $(objc [] (''Int <: [cexp| GHCTokenOcurly |]))
                  GHC.ITccurly         -> $(objc [] (''Int <: [cexp| GHCTokenCcurly |]))
                  GHC.ITvocurly        -> $(objc [] (''Int <: [cexp| GHCTokenVocurly |]))
                  GHC.ITvccurly        -> $(objc [] (''Int <: [cexp| GHCTokenVccurly |]))
                  GHC.ITobrack         -> $(objc [] (''Int <: [cexp| GHCTokenObrack |]))
                  GHC.ITopabrack       -> $(objc [] (''Int <: [cexp| GHCTokenOpabrack |]))
                  GHC.ITcpabrack       -> $(objc [] (''Int <: [cexp| GHCTokenCpabrack |]))
                  GHC.ITcbrack         -> $(objc [] (''Int <: [cexp| GHCTokenCbrack |]))
                  GHC.IToparen         -> $(objc [] (''Int <: [cexp| GHCTokenOparen |]))
                  GHC.ITcparen         -> $(objc [] (''Int <: [cexp| GHCTokenCparen |]))
                  GHC.IToubxparen      -> $(objc [] (''Int <: [cexp| GHCTokenOubxparen |]))
                  GHC.ITcubxparen      -> $(objc [] (''Int <: [cexp| GHCTokenCubxparen |]))
                  GHC.ITsemi           -> $(objc [] (''Int <: [cexp| GHCTokenSemi |]))
                  GHC.ITcomma          -> $(objc [] (''Int <: [cexp| GHCTokenComma |]))
                  GHC.ITunderscore     -> $(objc [] (''Int <: [cexp| GHCTokenUnderscore |]))
                  GHC.ITbackquote      -> $(objc [] (''Int <: [cexp| GHCTokenBackquote |]))
                  GHC.ITsimpleQuote    -> $(objc [] (''Int <: [cexp| GHCTokenSimpleQuote |]))
                  --
                  GHC.ITvarsym       _ -> $(objc [] (''Int <: [cexp| GHCTokenVarsym |]))
                  GHC.ITconsym       _ -> $(objc [] (''Int <: [cexp| GHCTokenConsym |]))
                  GHC.ITvarid        _ -> $(objc [] (''Int <: [cexp| GHCTokenVarid |]))
                  GHC.ITconid        _ -> $(objc [] (''Int <: [cexp| GHCTokenConid |]))
                  GHC.ITqvarsym      _ -> $(objc [] (''Int <: [cexp| GHCTokenQvarsym |]))
                  GHC.ITqconsym      _ -> $(objc [] (''Int <: [cexp| GHCTokenQconsym |]))
                  GHC.ITqvarid       _ -> $(objc [] (''Int <: [cexp| GHCTokenQvarid |]))
                  GHC.ITqconid       _ -> $(objc [] (''Int <: [cexp| GHCTokenQconid |]))
                  --
                  GHC.ITinteger      _ -> $(objc [] (''Int <: [cexp| GHCTokenInteger |]))
                  GHC.ITrational     _ -> $(objc [] (''Int <: [cexp| GHCTokenRational |]))
                  --
                  GHC.ITchar         _ -> $(objc [] (''Int <: [cexp| GHCTokenChar |]))
                  GHC.ITstring       _ -> $(objc [] (''Int <: [cexp| GHCTokenString |]))
                  GHC.ITlineComment  _ -> $(objc [] (''Int <: [cexp| GHCTokenLineComment |]))
                  GHC.ITblockComment _ -> $(objc [] (''Int <: [cexp| GHCTokenBlockComment |]))
                  _                    -> $(objc [] (''Int <: [cexp| GHCTokenOther |]))

          GHC.RealSrcLoc startLoc = GHC.srcSpanStart srcSpan
          GHC.RealSrcLoc endLoc   = GHC.srcSpanEnd srcSpan
          line                    = GHC.srcLocLine startLoc
          column                  = GHC.srcLocCol startLoc
          lines                   = GHC.srcLocLine endLoc - line + 1
          endColumn               = GHC.srcLocCol endLoc

mockNSArrayToTokenList :: NSArray (GHC.Located GHC.Token) -> IO [GHC.Located GHC.Token]
mockNSArrayToTokenList = error "mockNSArrayToTokenList"

objc_marshaller 'tokenListToNSArray 'mockNSArrayToTokenList

-- Turn a string of Haskell code into an array of tokens.
--
-- If tokenisation fails, the array is empty and the diagnostics handler is called with a suitable message.
--
-- tokeniseString :: Session -> String -> String -> IO (NSArray (GHC.Located GHC.Token))
tokeniseString :: Session -> String -> String -> Int -> Int -> IO [GHC.Located GHC.Token]
tokeniseString session text fname line column
  = tokenise session (GHC.stringToStringBuffer text) (GHC.mkRealSrcLoc (GHC.mkFastString fname) line column)

-- Load a module of which the actual program code is given. The backing path of the module is provided as well together with the 
-- list of search paths that should be used for module loading.
--
-- FIXME: Should be 'FilePath' instead of the first 'String', but language-c-inline doesn't see through type synonyms...
loadModuleText :: Session -> String -> [String] -> String -> IO Bool
loadModuleText session fname importPaths moduleText
  = do
    { utcTime <- getCurrentTime
    ; showResult <$> load session importPaths (target utcTime)
    }
  where
    showResult (Result _) = True
    showResult Error      = False
    
    target utcTime 
      = GHC.Target
        { GHC.targetId           = GHC.TargetFile fname Nothing
        , GHC.targetAllowObjCode = False
        , GHC.targetContents     = Just (GHC.stringToStringBuffer moduleText, utcTime)
        }

newtype NSObject = NSObject (ForeignPtr NSObject)
  deriving Typeable   -- needed for now until migrating to new TH
  
objc_typecheck

listOfNSObjectToNSArray :: [NSObject] -> IO (NSArray NSObject)
listOfNSObjectToNSArray objs
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray NSObject|] <: [cexp| [NSMutableArray arrayWithCapacity:50] |])
    ; mapM_ (addElement marr) objs
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr obj
      = $(objc ['marr :> Class [t|NSMutableArray NSObject|], 'obj :> Class ''NSObject] $ void 
          [cexp| (obj == nil) ? [marr addObject:[NSNull null]] : [marr addObject:obj] |])

mockListOfNSObjectToNSArray :: NSArray NSObject -> IO [NSObject]
mockListOfNSObjectToNSArray = error "mockListOfNSObjectToNSArray: shouldn't be executed"

objc_marshaller 'listOfNSObjectToNSArray 'mockListOfNSObjectToNSArray

evalText :: Session -> String -> Int -> String -> IO [NSObject]
evalText session source line stmtText
  | "import " `isPrefixOf` stmtText
  = do
    { executeImport session source line stmtText
        -- we are ignoring the result value for now
    ; return []
    }
  | any (flip isPrefixOf stmtText) declPrefixes
  = do 
    { result <- declare session source line stmtText
    ; case result of
        Error          -> return []
        Result strings -> mapM stringToNSObject ("":strings)
    }
  | otherwise
  = do
    { result <- eval session source line stmtText
    ; case result of
        Error                          -> return []
        Result (Left fptr, ty_strs)    -> (NSObject (castForeignPtr fptr):) <$> mapM stringToNSObject ty_strs
        Result (Right result, ty_strs) -> mapM stringToNSObject (result:ty_strs)
    }
  where
    stringToNSObject str 
      = do 
        { nsStr <- stringToNSString str
        ; let NSString fptr = nsStr
        ; return $ NSObject (castForeignPtr fptr)
        }

    -- As in GHCi: if one of these strings prefixes a command, then we treat it as a decl rather than a stmt.
    declPrefixes = ["class ","data ","newtype ","type ","instance ", "deriving ", "foreign ", "default ", "default("]


-- 'Interpreter.inferType' is not implemented yet.
--
-- typeText :: Session -> String -> Int -> String -> IO String
-- typeText session source line exprText 
--   = showResult <$> inferType session source line exprText
--   where
--     showResult (Result res) = res
--     showResult Error        = ""


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
- (instancetype)initWithDiagnosticsHandler:(DiagnosticsHandler)handler
               interactiveWorkingDirectory:(typename NSString *)workingDirectory;

/// Restart this GHC instance, interrupting any currently execution operation.
///
- (void)restart;

/// Turn a string of Haskell code into an array of tokens.
///
/// If tokenisation fails, the array is empty and the diagnostics handler is called with a suitable message.
///
- (typename NSArray *)tokeniseHaskell:(typename NSString*)text 
                                 file:(typename NSString *)file 
                                 line:(typename NSUInteger)line 
                               column:(typename NSUInteger)column;

/// Load a module given as a string with the file containing the module and the import search paths as context.
///
/// Any error messages and warnings are delivered via the diagnostics handler of the current object. The result
/// indicates whether loading was successful.
///
- (typename BOOL)loadModuleFromString:(typename NSString *)moduleText 
                                 file:(typename NSString *)file
                          importPaths:(typename NSArray/*<NSString>*/ *)importPaths;

/// Evaluate the Haskell expression given as a string. The result (if any) is accompanied by the types of all binders.
///
/// The result is either an `NSString` or an object representing the result. The types are string representations.
///
- (typename NSArray/*<NSObject>*/ *)evalExprFromString:(typename NSString *)exprText 
                                                source:(typename NSString *)source 
                                                  line:(typename NSUInteger)line;

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

objc_implementation [Typed 'startWithHandlerObject, Typed 'restart, Typed 'stop, Typed 'tokeniseString, Typed 'loadModuleText,
                     Typed 'evalText]
  [cunit|

@interface GHCInstance ()

// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr interpreterSession;

// The handler to process diagnostic messages arriving from GHC.
@property (strong) typename DiagnosticsHandler diagnosticsHandler;

// GHC log level from defaults.
@property typename NSInteger logLevel;

@end

typename NSString *kPreferenceGHCLogLevel = @"GHCLogLevel";     // Must match the HfM key.

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
  return [self initWithDiagnosticsHandler:nil interactiveWorkingDirectory:nil];
}

- (instancetype)initWithDiagnosticsHandler:(typename DiagnosticsHandler)handler 
              interactiveWorkingDirectory:(typename NSString *)workingDirectory
{
  self = [super init];
  if (self) {
    
    _logLevel = [[NSUserDefaults standardUserDefaults] integerForKey:kPreferenceGHCLogLevel];
    if (_logLevel)
      NSLog(@"GHC instance start");
    self.interpreterSession = startWithHandlerObject(self, _logLevel, workingDirectory);
    self.diagnosticsHandler  = handler;
    
  }
  return self;
}

- (void)dealloc
{
  stop(self.interpreterSession);
  if (_logLevel)
    NSLog(@"GHC instance stop");
  hs_free_stable_ptr(self.interpreterSession);
}

// Public model methods
// --

- (void)restart
{
  restart(self.interpreterSession);
}

- (typename NSArray *)tokeniseHaskell:(typename NSString*)text 
                                 file:(typename NSString *)file 
                                 line:(typename NSUInteger)line 
                               column:(typename NSUInteger)column
{
  return tokeniseString(self.interpreterSession, text, file, (typename NSInteger)line, (typename NSInteger)column);
}

- (typename BOOL)loadModuleFromString:(typename NSString *)moduleText 
                                 file:(typename NSString *)file
                          importPaths:(typename NSArray/*<NSString>*/ *)importPaths
{
  if (self.logLevel)
    NSLog(@"Loading module for %@", file);
  return loadModuleText(self.interpreterSession, file, importPaths, moduleText);
}

- (typename NSArray/*<NSObject>*/ *)evalExprFromString:(typename NSString *)exprText 
                                                source:(typename NSString *)source 
                                                  line:(typename NSUInteger)line
{
  return evalText(self.interpreterSession, source, (typename NSInteger)line, exprText);
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
