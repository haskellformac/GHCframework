{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-- |
-- Module    : Interpreter
-- Copyright : [2014..2015] Manuel M T Chakravarty
-- License   : All rights reserved
--
-- Maintainer: Manuel M T Chakravarty <chak@justtesting.org>
--
-- Haskell interpreter based on the GHC API.

module Interpreter (
  Session, Result(..), EvalResult,
  start, stop, tokenise, eval, inferType, executeImport, declare, load 
) where

  -- standard libraries
import Prelude                      hiding (catch)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception            (SomeException, evaluate)
import Control.Monad
import Data.Dynamic                 hiding (typeOf)
import Data.Either
import Data.Maybe
import Foreign                      hiding (void)
import System.CPUTime
import System.Directory
import System.FilePath
import System.IO
import Unsafe.Coerce                (unsafeCoerce)

  -- GHC
import qualified Bag          as GHC
import qualified DynFlags     as GHC
import qualified ErrUtils     as GHC
import qualified Exception    as GHC
import qualified HscTypes     as GHC
import qualified HsImpExp     as GHC
import qualified GHC          as GHC
import qualified GhcMonad     as GHC
import qualified HscMain      as GHC
import qualified InteractiveEvalTypes as GHC
import qualified Lexer        as GHC
import qualified Linker       as GHC
import qualified MonadUtils   as GHC
import qualified Name         as GHC
import qualified StringBuffer as GHC
import qualified PprTyThing   as GHC
import qualified Type         as GHC
import qualified Outputable   as GHC

  -- GHCKit
import PrintInterceptor


-- FIXME: temporary
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- Relative (to the 'GHC.framework' location) paths to the binary and library directory.
--
prefix, bindir, libdir :: String
prefix = "GHC.framework/Versions/Current/usr/"
bindir = prefix </> "bin"
libdir = prefix </> "lib/ghc-7.8.3"

-- Compiler wrapper
-- 
-- FIXME: Would be nicer to have 'ToolWrapper' in the GHC.framework/, but Xcode messes that up and generates a code signing error
ccWrapper :: String
ccWrapper = "../MacOS" </> "ToolWrapper"

-- |Abstract handle of an interpreter session.
--
data Session = Session 
               { inlet         :: (MVar (Maybe (GHC.Ghc ())))   -- inlet to submit tasks to the interpreter thread
               , logLevel      :: Int                           -- log level
               , logString     :: String -> IO ()               -- log function
               , sessionDflags :: GHC.DynFlags                  -- dflags as determined at session start
               }

-- |Possible results of executing an interpreter action. 
--
--FIXME: we want to make this asynchronous...
data Result r = Result r        -- ^Success or dynamic error, including error message as a string.
              | Error           -- ^Static error: error information is delivered asynchonously.

-- |Start a new interpreter session given a handler for the diagnostic messages arising in this session.
--
start :: FilePath                                           -- ^GHC.framework location (to locate the package database).
      -> (GHC.Severity -> GHC.SrcSpan -> String -> IO ())   -- ^Callback to report diagnostics.
      -> Int                                                -- ^Log level == GHC verbosity (0 == no logging)
      -> (String -> IO ())
      -> Maybe FilePath                                     -- ^Working directory for interactive Haskell statement execution.
      -> IO Session
start ghcBundlePath diagnosticHandler logLevel logString cwd
  = do
    { setNumCapabilities 2
    ; numCaps <- getNumCapabilities
    ; when (logLevel > 0) $
        logString $ "Starting interactive session (#capabilities = " ++ show numCaps ++ ")"

        -- Run GHC on a seperate OS thread. (Instead of using those that we get from GCD for the calls into Haskell.)
    ; inlet    <- newEmptyMVar
    ; resultMV <- newEmptyMVar
    ; forkOn 1 $ void $ GHC.runGhc (Just $ ghcBundlePath </> libdir) (startSession inlet resultMV)
    ; takeMVar resultMV
    }
  where
    startSession inlet resultMV
      = do
        { start <- GHC.liftIO $ getCPUTime
        
            -- Initialise the session by reading the package database
        ; dflags     <- GHC.getSessionDynFlags
        ; let settings = GHC.settings dflags
        ; packageIds <- GHC.setSessionDynFlags $ dflags 
                                                 { GHC.ghcLink          = GHC.LinkInMemory
                                                 , GHC.hscTarget        = GHC.HscInterpreted
                                                 , GHC.settings         = settings {GHC.sPgm_c = (ghcBundlePath </> ccWrapper, 
                                                                                                  snd . GHC.sPgm_c $ settings)}
                                                 , GHC.log_action       = logAction
                                                 , GHC.extraPkgConfs    = const [GHC.GlobalPkgConf]
                                                 , GHC.packageFlags     = [GHC.ExposePackage "ghckit-support"]
                                                 , GHC.verbosity        = logLevel
                                                 } 
                                                 `GHC.xopt_set`   GHC.Opt_ExtendedDefaultRules        -- extra options for..
                                                 `GHC.xopt_unset` GHC.Opt_MonomorphismRestriction     -- interactive evaluation
                                                 -- `GHC.gopt_unset` GHC.Opt_GhciSandbox  -- currently ignored in `runStmt` anyway
        ; sessionDFlags <- GHC.getSessionDynFlags
        ; let session = Session inlet logLevel logString sessionDFlags
        ; GHC.liftIO $ putMVar resultMV session    -- send caller home as quickly as possible
        ; logMsg session $ "Session packages: " ++ GHC.showSDoc dflags (GHC.pprQuotedList packageIds)

            -- Load 'ghckit-support' and...
        ; GHC.load GHC.LoadAllTargets
        -- ; unless (???successfully loaded???) $
        --     error "PANIC: could not initialise 'ghckit-support'"

            -- ...initialise the interactive print channel
        ; interceptor <- GHC.parseImportDecl "import PrintInterceptor"
        ; GHC.setContext [GHC.IIDecl interceptor]
        ; [printInterceptor] <- GHC.parseName "PrintInterceptor.interactivePrintInterceptor"
        ; GHC.modifySession (\he -> let new_ic = GHC.setInteractivePrintName (GHC.hsc_IC he) printInterceptor
                                    in he {GHC.hsc_IC = new_ic})
        ; maybe_chan <- fromDynamic <$> GHC.dynCompileExpr "PrintInterceptor.interactivePrintChan"
        ; case maybe_chan of
            Nothing   -> error "PANIC: could not get 'interactivePrintChan'"
            Just chan -> GHC.liftIO $ writeIORef chanRef chan
            
            -- Set the working directory for Haskell evaluation.
        ; logMsg session $ "current working directory at GHC session start = " ++ show cwd
        ; GHC.modifySession $ \hsc ->
            let ic = GHC.hsc_IC hsc in hsc { GHC.hsc_IC = ic { GHC.ic_cwd = cwd } }

        ; logTiming session start "GHC session initialisation"

        ; runSession inlet
        }

    logAction :: GHC.LogAction
    logAction dflags severity srcSpan style msg
      = diagnosticHandler severity srcSpan (GHC.renderWithStyle dflags msg style)
        
    runSession inlet
      = do
        { maybeCommand <- GHC.liftIO $ takeMVar inlet
        ; case maybeCommand of
            Nothing      -> return ()
            Just command -> command >> runSession inlet
        }

-- Terminate an interpreter session.
--
stop :: Session -> IO ()
stop session = putMVar (inlet session) Nothing

-- Tokenise a string of Haskell code.
--
-- NB: In contrast to all other operations, we tokenise on the current thread and don't dispatch to our GHC worker. Tokenisation
--     needs to be fast for syntax highlighting and must not have to wait for any currently ongoing statement evaluation.
--
tokenise :: Session -> GHC.StringBuffer -> GHC.RealSrcLoc -> IO [GHC.Located GHC.Token]
tokenise session strbuf loc
  = do
    { start <- getCPUTime
    ; let result = GHC.lexTokenStream strbuf loc dflags
    ; result `seq` logTimingIO session start "tokenise"
    ; case result of
        GHC.POk _pstate tokens -> return tokens
        GHC.PFailed loc msg    -> do
        { GHC.log_action dflags dflags GHC.SevError loc (GHC.defaultErrStyle dflags) msg
        ; return []
        }
    }
  where
    dflags = sessionDflags session

-- The result of evaluation is usually a 'show'ed result together with the types of binders. However, it can also be a
-- reference to an Objective-C representation of the result (again with type information in the second component).
--
type EvalResult = Result (Either (ForeignPtr ()) String, [String])

-- Evaluate a Haskell expression in the given interpreter session, 'show'ing its result. Also return the types of all binders
-- (being 'it' in the case of a vanilla expression). Alternatively, the result may be a reference to an Objective-C
-- representation of the result.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
eval :: Session -> String -> Int -> String -> IO EvalResult
eval session source line stmt
  = tryGhcAction (inlet session) (\msg -> Result (Right msg, [])) $
        GHC.handleSourceError (\e -> handleError e >> return Error) $ do
        { logCode session $ "evaluating: «" ++ stmt ++ "»"
        
            -- if SpriteKit is available, test for return types that we render specially
        ; isSpriteKitAvailable <- isRight <$> (GHC.gtry $ 
            GHC.runStmtWithLocation source line "Graphics.SpriteKit.spritekit_initialise" GHC.RunToCompletion
            :: GHC.Ghc (Either GHC.SourceError GHC.RunResult))
        ; colorNames   <- if isSpriteKitAvailable then GHC.parseName "Graphics.SpriteKit.Color"   else return []
        ; skImageNames <- if isSpriteKitAvailable then GHC.parseName "Graphics.SpriteKit.Image"   else return []
        ; textureNames <- if isSpriteKitAvailable then GHC.parseName "Graphics.SpriteKit.Texture" else return []
        ; nodeNames    <- if isSpriteKitAvailable then GHC.parseName "Graphics.SpriteKit.Node"    else return []
        ; sceneNames   <- if isSpriteKitAvailable then GHC.parseName "Graphics.SpriteKit.Scene"   else return []
        
            -- if JuicyPixels is available, test for image return types
        ; imageNames        <- ((++) <$> GHC.parseName "Codec.Picture.Image" 
                                     <*> GHC.parseName "Codec.Picture.DynamicImage")
                               `GHC.gcatch` \(_exc :: GHC.SourceError) -> return []

            -- try to determine the type of the statement if it is an expression
        ; ty <- GHC.gtry $ GHC.exprType stmt
        ; case ty :: Either GHC.SourceError GHC.Type of
            Right ty -> do
              { ty_str <- renderType ty
              ; case GHC.tyConAppTyCon_maybe . GHC.dropForAlls $ ty of    -- look through type synonyms & extract the base type
                  Just tycon 
                    | GHC.getName tycon `elem` colorNames   -> runNodeEval "Graphics.SpriteKit.colorToForeignPtr"   ty_str
                    | GHC.getName tycon `elem` skImageNames -> runNodeEval "Graphics.SpriteKit.imageToForeignPtr"   ty_str
                    | GHC.getName tycon `elem` textureNames -> runNodeEval "Graphics.SpriteKit.textureToForeignPtr" ty_str
                    | GHC.getName tycon `elem` nodeNames    -> runNodeEval "Graphics.SpriteKit.nodeToForeignPtr"    ty_str
                    | GHC.getName tycon `elem` sceneNames   -> runNodeEval "Graphics.SpriteKit.sceneToForeignPtr"   ty_str
                    | GHC.getName tycon `elem` imageNames   -> runImageEval isSpriteKitAvailable ty_str
                  _                                         -> runGHCiStatement (Just ty_str)
              }
            Left _e -> runGHCiStatement Nothing
        }
  where
    runImageEval isSpriteKitAvailable ty_str
      | not isSpriteKitAvailable
      = return $ Result (Right $ "** Exception: import Graphics.SpriteKit to render images", [ty_str])
      | otherwise
      = do
        { logMsg session "evaluating JuicyPixels image"
        
        ; runNodeEval "Graphics.SpriteKit.imageToForeignPtr" ty_str
        }
      
    runNodeEval conversionName ty_str
      = do
        { logMsg session ("evaluating SpriteKit with " ++ conversionName)
        ; start <- GHC.liftIO $ getCPUTime
        
            -- result is a SpriteKit node => get a reference to that node instead of pretty printing
        ; let nodeExpr = conversionName ++ " " ++ parens stmt
        ; hval <- GHC.compileExpr nodeExpr
        -- FIXME: we need to sandbox this as in GHCi's 'InteractiveEval.sandboxIO'
        ; result <- withVirtualCWD $                                    -- code executon needs to use the IC working directory
                      GHC.liftIO $
                        GHC.try $ 
                          (unsafeCoerce hval :: IO (ForeignPtr ()))     -- force evaluation => contain effects & catch exceptions
        ; logTiming session start "evaluate node"

        -- ; GHC.setContext iis
        ; case result of
            Right result -> return $ Result (Left result, [ty_str])
            Left  exc    -> return $ Result (Right $ "** Exception: " ++ show (exc :: SomeException), [ty_str])
        }
        
    runGHCiStatement maybe_ty_str
      = GHC.handleSourceError (\e -> case maybe_ty_str of
                                       Nothing     -> handleError e >> return Error
                                       Just ty_str -> return $ Result (Right "«cannot show values of this type»", [ty_str])) $ do
        { logMsg session "evaluating general statement"
        ; start <- GHC.liftIO $ getCPUTime
 
            -- GHCi-style command execution
        ; runResult <- runStmt session source line stmt
        ; logTiming session start "evaluate general statement"
        ; case runResult of
            GHC.RunOk names       -> do
                                     { chan        <- GHC.liftIO $ readIORef chanRef
                                     ; maybe_value <- GHC.liftIO $ atomically $ tryReadTChan chan
                                     ; types       <- renderTypesOfNames names
                                     ; if isNothing maybe_value && null names
                                       then
                                         return $ Result (Right "", maybeToList maybe_ty_str)
                                       else
                                         return $ Result (Right $ fromMaybe "" maybe_value, types)
                                     }
            GHC.RunException exc  -> return $ Result (Right $ "** Exception: " ++ show exc, maybeToList maybe_ty_str)
            _                     -> return $ Result (Right $ "** Exception: <unexpected break point>", maybeToList maybe_ty_str)
        }
    --
    parens s = "(let {interpreter'binding_ =\n" ++ s ++ "\n ;} in interpreter'binding_)"

-- Exectute a statement much like GHC API's `runStmtWithLocation`, but omit breakpoint handling.
--
-- FIXME: We can probaby get rid of this again. However, sandboxing uses forkIO, which allows the interpreted computation 
--        to pick the capability that we got via the in-call from Swift-land. We'd rather run all interpreted code on a 
--        seperate capability. Maybe we need out own sandbox code that uses `forkOn`...(then, we can use that for node evaluation,
--        too!
runStmt :: Session -> String -> Int -> String -> GHC.Ghc GHC.RunResult
runStmt session source line stmt
  = do
    { hsc_env <- GHC.getSession
    
        -- Turn off -fwarn-unused-bindings when running a statement, to hide
        -- warnings about the implicit bindings we introduce.
    ; let ic       = GHC.hsc_IC hsc_env -- use the interactive dflags
          idflags' = GHC.ic_dflags ic `GHC.wopt_unset` GHC.Opt_WarnUnusedBinds
          hsc_env' = hsc_env{ GHC.hsc_IC = ic{ GHC.ic_dflags = idflags' } }

        -- Compile to value (IO [HValue]), don't run
    ; logMsg session "runStmt: compiling"
    ; r <- GHC.liftIO $ GHC.hscStmtWithLocation hsc_env' stmt source line

    ; case r of
        Nothing -> return (GHC.RunOk [])        -- Empty statement or comment
    
        Just (tyThings, hval, fix_env) -> do
          { updateFixityEnv fix_env
             -- FIXME: do we want to sandbox this as in GHCi's 'InteractiveEval.sandboxIO'?
          ; logMsg session "runStmt: running"
          ; status <- withVirtualCWD $
                        GHC.liftIO $ 
                          GHC.Complete <$>
                            GHC.try hval
          ; logMsg session "runStmt: done"
          ; case status of
              GHC.Break {}               -> return $ GHC.RunException breakExc   -- Hit a breakpoint — this shouldn't happen!
              GHC.Complete (Left e)      -> return $ GHC.RunException e          -- Completed with an exception — report it!
              GHC.Complete (Right hvals) -> do                                   -- Completed successfully — update environments!
                { let ic          = GHC.hsc_IC hsc_env
                      bindings    = (GHC.ic_tythings ic, GHC.ic_rn_gbl_env ic)
                      final_ic    = GHC.extendInteractiveContext (GHC.hsc_IC hsc_env) (map GHC.AnId tyThings)
                      final_names = map GHC.getName tyThings
                ; GHC.liftIO $ GHC.extendLinkEnv (zip final_names hvals)
                -- FIXME: rttiEnvironment is not exported by GHC API — needed once we want to inspect runtime types
                -- ; hsc_env' <- liftIO $ GHC.rttiEnvironment hsc_env{ GHC.hsc_IC = final_ic }  
                ; let hsc_env' = hsc_env{ GHC.hsc_IC = final_ic }
                ; GHC.modifySession (\_ -> hsc_env')
                ; return $ GHC.RunOk final_names
                }
          }
    }
  where
    breakExc = GHC.toException $ GHC.ErrorCall "unexpected breakpoint"
    
    updateFixityEnv :: GHC.GhcMonad m => GHC.FixityEnv -> m ()
    updateFixityEnv fix_env 
      = do
        { hsc_env <- GHC.getSession
        ; let ic = GHC.hsc_IC hsc_env
        ; GHC.setSession $ hsc_env { GHC.hsc_IC = ic { GHC.ic_fix_env = fix_env } }
        }

-- Infer the type of a Haskell expression in the given interpreter session.
--
-- If GHC raises an error, we pretty print it.
--
-- FIXME: improve error reporting
inferType :: Session -> String -> Int -> String -> IO (Result String)
inferType = error "inferType is not implemented"

-- Execute an import statement in the given interpreter session.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
executeImport :: Session -> String -> Int -> String -> IO (Result ())
executeImport session _source _line stmt
  = do
    { resultMV <- newEmptyMVar
    ; putMVar (inlet session) $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (\e -> handleError e >> GHC.liftIO (putMVar resultMV Error)) $ do
        { start <- GHC.liftIO $ getCPUTime
        ; iis <- GHC.getContext
        ; importDecl <- GHC.parseImportDecl stmt
        ; GHC.setContext (GHC.IIDecl importDecl : iis)

        ; logTiming session start stmt

            -- Communicate the result back to the main thread
        ; GHC.liftIO $ 
            putMVar resultMV (Result ())
        }
    ; takeMVar resultMV
    }

-- Bring a set of delcrations into scope in the given interpreter session.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
declare :: Session -> String -> Int -> String -> IO (Result [String])
declare session source line stmt
  = do
    { resultMV <- newEmptyMVar
    ; putMVar (inlet session) $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (\e -> handleError e >> GHC.liftIO (putMVar resultMV Error)) $ do
        { start <- GHC.liftIO $ getCPUTime
        ; names <- GHC.runDeclsWithLocation source line stmt
        ; types <- renderTypesOfNames names

       ; logTiming session start "compile declaration"

            -- Communicate the result back to the main thread
        ; GHC.liftIO $ 
            putMVar resultMV (Result types)
        }
    ; takeMVar resultMV
    }

-- Load a module into in the given interpreter session.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
load :: Session -> [String] -> GHC.Target -> IO (Result ())
load session importPaths target
  = do
    { resultMV <- newEmptyMVar
    ; putMVar (inlet session) $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (\e -> handleError e >> GHC.liftIO (putMVar resultMV Error)) $ do
        { start <- GHC.liftIO $ getCPUTime
        
            -- Save the working directory for Haskell evaluation.
        ; cwd <- GHC.withSession $ return . GHC.ic_cwd . GHC.hsc_IC
        
            -- Revert CAFs from previous evaluations
        ; GHC.liftIO $ rts_revertCAFs
        
            -- Set the search paths
        ; modifyDynFlags $ \dflags -> dflags { GHC.importPaths = importPaths }

            -- Load the new target
        ; GHC.setTargets [target]
        ; GHC.load GHC.LoadAllTargets
        
        ; logTiming session start "loading"

            -- Set the GHC context if loading was successful (to support subsequent expression evaluation)
        ; loadedModules <- map GHC.ms_mod_name <$> GHC.getModuleGraph >>= filterM GHC.isLoaded
        ; case loadedModules of
            modname:_ -> do
                         {   -- Intercept the interactive printer to get evaluation results
                             -- (NB: We need to do that again after every 'GHC.load', as that scraps the interactive context.)
                         ; interceptor <- GHC.parseImportDecl "import PrintInterceptor"
                         ; GHC.setContext [GHC.IIDecl interceptor]
                         ; [printInterceptor] <- GHC.parseName "PrintInterceptor.interactivePrintInterceptor"
                         ; dflags <- GHC.getSessionDynFlags
                         ; GHC.modifySession (\he -> let new_ic = GHC.setInteractivePrintName (GHC.hsc_IC he) printInterceptor
                                                     in he {GHC.hsc_IC = new_ic})

                             -- Only make the user module available for interactive use
                         ; GHC.setContext [GHC.IIModule modname]    -- FIXME: so far, we only try to 

                             -- Communicate the result back to the main thread
                         ; GHC.liftIO $ 
                             putMVar resultMV (Result ())
                         }
            []        -> GHC.liftIO $ putMVar resultMV Error

            -- Set the working directory for Haskell evaluation again in the new interactive context.
        ; GHC.modifySession $ \hsc ->
            let ic = GHC.hsc_IC hsc in hsc { GHC.hsc_IC = ic { GHC.ic_cwd = cwd } }
        }
    ; takeMVar resultMV
    }

-- Inject the error messages contained in the given 'SourceError' exception into the asynchronous diagnostics stream.
-- 
handleError :: GHC.GhcMonad m => GHC.SourceError -> m ()
handleError e
  = do
    { dflags <- GHC.getSessionDynFlags
    ; mapM_ (dispatchErrMsg dflags) (GHC.bagToList . GHC.srcErrorMessages $ e)
    }
  where
    dispatchErrMsg dflags errMsg 
      = GHC.liftIO $ GHC.log_action dflags dflags GHC.SevError srcSpan (GHC.mkErrStyle dflags unqual) doc
      where
        srcSpan = GHC.errMsgSpan errMsg
        doc     = GHC.errMsgShortDoc errMsg GHC.$$ GHC.errMsgExtraInfo errMsg
        unqual  = GHC.errMsgContext errMsg

chanRef :: IORef (TChan String)
{-# NOINLINE chanRef #-}
chanRef = unsafePerformIO $ newIORef (error "Interpreter.chanRef not yet initialised")


-- Utitlity functions
-- ------------------

modifyDynFlags :: (GHC.DynFlags -> GHC.DynFlags) -> GHC.Ghc ()
modifyDynFlags f = GHC.getSessionDynFlags >>= void . GHC.setSessionDynFlags . f

logMsg :: Session -> String -> GHC.Ghc ()
logMsg = logMsgFromLevel 1

logCode :: Session -> String -> GHC.Ghc ()
logCode = logMsgFromLevel 2

logMsgFromLevel :: Int -> Session -> String -> GHC.Ghc ()
logMsgFromLevel minLogLevel session msg = GHC.liftIO $ logMsgFromLevelIO minLogLevel session msg

logMsgFromLevelIO :: Int -> Session -> String -> IO ()
logMsgFromLevelIO minLogLevel (Session{..}) msg
  | logLevel >= minLogLevel
  = logString ("[Interpreter] " ++ msg)
  | otherwise
  = return ()
  
logTiming :: Session -> Integer -> String -> GHC.Ghc ()
logTiming session start what = GHC.liftIO $ logTimingIO session start what
    
logTimingIO :: Session -> Integer -> String -> IO ()
logTimingIO session start what
  = do
    { end <- getCPUTime
    ; let deltaSeconds = fromIntegral (end - start) / 1e12 :: Double
    ; logMsgFromLevelIO 1 session $ "Elapsed time: " ++ show deltaSeconds ++ "s (" ++ what ++ ")"
    }

-- Wrap a command to execute in the context of the working directory of the interactive context.
--
-- GHC does that for 'runStmtWithLocation', but we need that for other ways of executing Haskel code, too.
--
withVirtualCWD :: GHC.GhcMonad m => m a -> m a
withVirtualCWD m 
  = do
    { hsc <- GHC.getSession
    ; let ic = GHC.hsc_IC hsc

    ; let set_cwd 
            = do
              { dir <- GHC.liftIO $ getCurrentDirectory
              ; case GHC.ic_cwd ic of
                  Just dir -> GHC.liftIO $ setCurrentDirectory dir
                  Nothing  -> return ()
              ; return dir
              }
          reset_cwd orig_dir 
            = do
              { virt_dir <- GHC.liftIO $ getCurrentDirectory
              ; hsc <- GHC.getSession
              ; GHC.setSession $ hsc { GHC.hsc_IC = (GHC.hsc_IC hsc) { GHC.ic_cwd = Just virt_dir } }
              ; GHC.liftIO $ setCurrentDirectory orig_dir
              }

    ; GHC.gbracket set_cwd reset_cwd $ const m
    }

-- Execute the given given GHC action in the interpreter thread through the inlet (first argument). Gracefully handle all
-- exceptions and ensure that the action is terminated within a fixed timeframe.
--
-- FIXME: This is currently only being used for evaluation. Other GHC actions should be protected, too.
tryGhcAction :: MVar (Maybe (GHC.Ghc ())) -> (String -> result) -> GHC.Ghc result -> IO result
tryGhcAction inlet errMsgHandler action
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $ do    -- the interpreter command we send over to the interpreter thread
        { tid <- GHC.liftIO $ myThreadId
        -- ; watchdogTid <- GHC.liftIO $ forkIO $ threadDelay 2000000 >> throwTo tid GHC.UserInterrupt
        ; watchdogTid <- GHC.liftIO $ forkIO $ threadDelay 20000000 >> throwTo tid GHC.UserInterrupt
        ; result <- GHC.gtry $ do {result <- action; GHC.liftIO $ killThread watchdogTid; return result}
        ; case result of
            Left exc -> 
              case GHC.fromException exc of
                Just GHC.UserInterrupt -> 
                  GHC.liftIO $ putMVar resultMV (errMsgHandler $ "** Exception: evaluation timed out (took too long)")
                Just asyncExc -> 
                  GHC.liftIO $ putMVar resultMV (errMsgHandler $ "** Exception: " ++ show asyncExc)
                Nothing -> 
                  GHC.liftIO $ putMVar resultMV (errMsgHandler $ "** Exception: " ++ show (exc :: SomeException))
            Right result -> GHC.liftIO $ putMVar resultMV result
        }
    ; takeMVar resultMV
    }

-- Given a set of names, pretty print the bound 'TyThing' headers.
--
renderTypesOfNames :: [GHC.Name] -> GHC.Ghc [String]
renderTypesOfNames names  
  = do
    { dflags <- GHC.getDynFlags
    ; unqual <- GHC.getPrintUnqual
    ; mapM (\name -> typeOf dflags unqual name) names
    }
  where
    typeOf dflags unqual name
      = do
        { maybe_tyThing <- GHC.lookupName name
        ; case maybe_tyThing of
            Nothing      -> return ""
            Just tyThing -> return $ GHC.showSDocForUser dflags unqual (GHC.pprTyThingHdr tyThing)
        }

renderType :: GHC.Type -> GHC.Ghc String
renderType ty
  = do
    { dflags <- GHC.getDynFlags
    ; unqual <- GHC.getPrintUnqual
    ; return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser ty)
    }


-- Runtime system support
-- ----------------------
foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case
