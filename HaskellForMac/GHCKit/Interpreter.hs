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
  start, stop, eval, typeOf, load  
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
import System.IO

  -- GHC
import qualified DynFlags   as GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified GHC        as GHC
import qualified GHC.Paths  as Paths
import qualified GhcMonad   as GHC
import qualified MonadUtils as GHC
import qualified Outputable as GHC

  -- GHCKit
import PrintInterceptor


-- FIXME: temporary
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- |Abstract handle of an interpreter session.
--
newtype Session = Session (MVar (Maybe (GHC.Ghc ())))

-- |Possible results of executing an interpreter action. 
--
--FIXME: we want to make this asynchronous...
data Result = Result String
            | Error  String

-- |Start a new interpreter session given a handler for the diagnostic messages arising in this session.
--
start :: (GHC.Severity -> GHC.SrcSpan -> String -> IO ()) -> IO Session
start diagnosticHandler
  = do
    { inlet <- newEmptyMVar
    ; forkIO $ void $ GHC.runGhc (Just Paths.libdir) (startSession inlet)
    ; return $ Session inlet
    }
  where
    startSession inlet 
      = do
        {   -- Initialise the session by reading the package database
        ; dflags     <- GHC.getSessionDynFlags
        ; packageIds <- GHC.setSessionDynFlags $ dflags 
                                                 { GHC.hscTarget        = GHC.HscInterpreted
                                                 , GHC.ghcLink          = GHC.LinkInMemory
                                                 , GHC.log_action       = logAction
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
    logAction dflags severity srcSpan _style msg
      = diagnosticHandler severity srcSpan (GHC.showSDoc dflags msg)
        
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

-- Evaluate a Haskell expression in the given interpreter session, 'show'ing its result.
--
-- If GHC raises an error, we pretty print it.
--
-- FIXME: improve error reporting
eval :: Session -> String -> IO Result
eval (Session inlet) stmt
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (handleError resultMV) $ do
        { runResult <- GHC.runStmt stmt GHC.RunToCompletion
        ; result <- case runResult of
                     GHC.RunOk _names      -> do
                                              { chan  <- GHC.liftIO $ readIORef chanRef
                                              ; maybe_value <- GHC.liftIO $ atomically $ tryReadTChan chan
                                              ; return $ Result (fromMaybe "<no result>" maybe_value)
                                              }
                     GHC.RunException _exc -> return $ Error "<exception>"
                     _                     -> return $ Error "<unexpected break point>"
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
-- If GHC raises an error, we pretty print it.
--
load :: Session -> GHC.Target -> IO Result
load (Session inlet) target
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        GHC.handleSourceError (handleError resultMV) $ do
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
            []        -> GHC.liftIO $ putMVar resultMV (Error "")
        }
    ; takeMVar resultMV
    }

handleError resultMV e
  = do
    { dflags <- GHC.getSessionDynFlags
    ; GHC.liftIO $ putMVar resultMV (Error $ pprError dflags e)
    }

pprError :: GHC.DynFlags -> GHC.SourceError -> String
pprError dflags err
  = GHC.showSDoc dflags (GHC.vcat $ GHC.pprErrMsgBagWithLoc (GHC.srcErrorMessages err))

chanRef :: IORef (TChan String)
{-# NOINLINE chanRef #-}
chanRef = unsafePerformIO $ newIORef (error "Interpreter.chanRef not yet initialised")


-- Runtime system support
-- ----------------------
foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case
