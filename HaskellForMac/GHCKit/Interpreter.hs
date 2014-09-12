-- |
-- Module    : Interpreter
-- Copyright : [2014] Manuel M T Chakravarty
-- License   : All rights reserved
--
-- Maintainer: Manuel M T Chakravarty <chak@justtesting.org>
--
-- Haskell interpreter based on the GHC API.

module Interpreter (
  Session, Result(..),
  start, stop, tokenise, eval, typeOf, load 
) where

  -- standard libraries
import Prelude                      hiding (catch)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception            (SomeException, evaluate)
import Control.Monad
import Data.Dynamic  hiding (typeOf)
import Data.Maybe
import System.FilePath
import System.IO

  -- GHC
import qualified Bag          as GHC
import qualified DynFlags     as GHC
import qualified ErrUtils     as GHC
import qualified HscTypes     as GHC
import qualified GHC          as GHC
import qualified GHC.Paths    as Paths
import qualified GhcMonad     as GHC
import qualified Lexer        as GHC
import qualified MonadUtils   as GHC
import qualified StringBuffer as GHC
import qualified Outputable   as GHC

  -- GHCKit
import PrintInterceptor


-- FIXME: temporary
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- Relative (to the 'GHC.bundle' location) path to the library directory.
--
libdir :: String
libdir = "GHC.bundle/Contents/usr/lib/ghc-7.8.3"

-- |Abstract handle of an interpreter session.
--
newtype Session = Session (MVar (Maybe (GHC.Ghc ())))

-- |Possible results of executing an interpreter action. 
--
--FIXME: we want to make this asynchronous...
data Result = Result String
            | Error

-- |Start a new interpreter session given a handler for the diagnostic messages arising in this session.
--
-- The file path specifies the 'GHC.bundle' location (from where we locate the package database).
--
start :: FilePath -> (GHC.Severity -> GHC.SrcSpan -> String -> IO ()) -> IO Session
start ghcBundlePath diagnosticHandler
  = do
    { inlet <- newEmptyMVar
    ; forkIO $ void $ GHC.runGhc (Just $ ghcBundlePath </> libdir) (startSession inlet)
    ; return $ Session inlet
    }
  where
    startSession inlet 
      = do
        {   -- Initialise the session by reading the package database
        ; dflagsRaw  <- GHC.getSessionDynFlags
        -- FIXME: the following doesn't help with OpenGL in the Playground
        ; let dflags  = GHC.gopt_unset dflagsRaw GHC.Opt_GhciSandbox
        ; packageIds <- GHC.setSessionDynFlags $ dflags 
                                                 { GHC.hscTarget        = GHC.HscInterpreted
                                                 , GHC.ghcLink          = GHC.LinkInMemory
                                                 , GHC.log_action       = logAction
                                                 , GHC.extraPkgConfs    = const [GHC.GlobalPkgConf]
                                                 , GHC.packageFlags     = [GHC.ExposePackage "ghckit-support"]
                                                 }
        ; GHC.liftIO $ 
            putStrLn $ "Session packages: " ++ GHC.showSDoc dflags (GHC.pprQuotedList packageIds)  -- FIXME: needs proper logging

            -- Load 'ghckit-support' and...
        ; GHC.load GHC.LoadAllTargets
        -- ; unless (???successfully loaded???) $
        --     error "PANIC: could not initialise 'ghckit-support'"

            -- ...initialise the interactive print channel
        ; interceptor <- GHC.parseImportDecl "import PrintInterceptor"
        ; GHC.setContext [GHC.IIDecl interceptor]
        ; [printInterceptor] <- GHC.parseName "PrintInterceptor.interactivePrintInterceptor"
        ; dflags <- GHC.getSessionDynFlags
        ; GHC.modifySession (\he -> let new_ic = GHC.setInteractivePrintName (GHC.hsc_IC he) printInterceptor
                                    in he {GHC.hsc_IC = new_ic})
        ; maybe_chan <- fromDynamic <$> GHC.dynCompileExpr "PrintInterceptor.interactivePrintChan"
        ; case maybe_chan of
            Nothing   -> error "PANIC: could not get 'interactivePrintChan'"
            Just chan -> GHC.liftIO $ writeIORef chanRef chan

        ; session inlet
        }

    logAction :: GHC.LogAction
    logAction dflags severity srcSpan style msg
      = diagnosticHandler severity srcSpan (GHC.renderWithStyle dflags msg style)
        
    session inlet
      = do
        { maybeCommand <- GHC.liftIO $ takeMVar inlet
        ; case maybeCommand of
            Nothing      -> return ()
            Just command -> command >> session inlet
        }

-- Terminate an interpreter session.
--
stop :: Session -> IO ()
stop (Session inlet) = putMVar inlet Nothing

-- Tokenise a string of Haskell code.
--
tokenise :: Session -> GHC.StringBuffer -> GHC.RealSrcLoc -> IO [GHC.Located GHC.Token]
tokenise (Session inlet) strbuf loc
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $ do
        { dflags <- GHC.getSessionDynFlags
        ; let result = GHC.lexTokenStream strbuf loc dflags
        ; case result of
            GHC.POk _pstate tokens -> GHC.liftIO $ putMVar resultMV tokens
            GHC.PFailed loc msg    -> do
            { GHC.liftIO $ GHC.log_action dflags dflags GHC.SevError loc (GHC.defaultErrStyle dflags) msg
            ; GHC.liftIO $ putMVar resultMV []
            }
        }      
    ; takeMVar resultMV
    }

-- Evaluate a Haskell expression in the given interpreter session, 'show'ing its result.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
eval :: Session -> String -> Int -> String -> IO Result
eval (Session inlet) source line stmt
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (\e -> handleError e >> GHC.liftIO (putMVar resultMV Error)) $ do
        { runResult <- GHC.runStmtWithLocation source line stmt GHC.RunToCompletion
        ; result <- case runResult of
                     GHC.RunOk _names      -> do
                                              { chan  <- GHC.liftIO $ readIORef chanRef
                                              ; maybe_value <- GHC.liftIO $ atomically $ tryReadTChan chan
                                              ; return $ Result (fromMaybe "" maybe_value)
                                              }
                     GHC.RunException _exc -> return $ Result "<exception>"
                     _                     -> return $ Result "<unexpected break point>"
        ; GHC.liftIO $ putMVar resultMV result
        }
    ; takeMVar resultMV
    }

-- Infer the type of a Haskell expression in the given interpreter session.
--
-- If GHC raises an error, we pretty print it.
--
-- FIXME: improve error reporting
typeOf :: Session -> String -> IO Result
typeOf = error "typeOf is not implemented"

-- Load a module into in the given interpreter session.
--
-- GHC errors are reported asynchronously through the diagnostics handler.
--
load :: Session -> GHC.Target -> IO Result
load (Session inlet) target
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (\e -> handleError e >> GHC.liftIO (putMVar resultMV Error)) $ do
        {
            -- Revert CAFs from previous evaluations
        ; GHC.liftIO $ rts_revertCAFs

            -- Load the new target
        ; GHC.setTargets [target]
        ; GHC.load GHC.LoadAllTargets

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
                             putMVar resultMV (Result "")
                         }
            []        -> GHC.liftIO $ putMVar resultMV Error
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


-- Runtime system support
-- ----------------------
foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case
