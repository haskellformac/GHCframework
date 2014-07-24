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
import Control.Exception            (SomeException, evaluate)
import Control.Monad
import Control.Monad.Catch
import System.IO

  -- GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified GHC        as GHC
import qualified GHC.Paths  as Paths
import qualified MonadUtils as GHC
import qualified Outputable as GHC

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

-- |Start a new interpreter session.
--
start :: IO Session
start
  = do
    { inlet <- newEmptyMVar
    ; forkIO $ void $ GHC.runGhc (Just Paths.libdir) (startSession inlet)
    ; return $ Session inlet
    }
  where
    startSession inlet 
      = do
        {   -- Initialise the session by reading the package database
        ; dflags <- GHC.getSessionDynFlags
        ; _packageIds <- GHC.setSessionDynFlags $ dflags 
                                                  { GHC.hscTarget  = GHC.HscInterpreted
                                                  , GHC.ghcLink    = GHC.LinkInMemory
                                                  , GHC.log_action = logAction
                                                  }
        ; session inlet
        }
        
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
                     GHC.RunOk names       -> do
                                              { dflags <- GHC.getSessionDynFlags
                                              ; let namesStr = GHC.showSDoc dflags (GHC.pprQuotedList names)
                                              ; return $ Result ("(" ++ namesStr ++ ")")
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

            -- Determine errors if unsuccessful
        ; msgs <- reverse <$> GHC.liftIO (readIORef logRef)
        ; GHC.liftIO $ writeIORef logRef []
        ; let result | null msgs = Result "Loaded module successfully."
                     | otherwise = Error $ concat msgs

            -- Set the GHC context if loading was successful (to support subsequent expression evaluation)
        ; loadedModules <- map GHC.ms_mod_name <$> GHC.getModuleGraph >>= filterM GHC.isLoaded
        ; case loadedModules of
            modname:_ -> GHC.setContext [GHC.IIModule modname]    -- FIXME: so far, we only try to load one module
            []        -> return ()
            
            -- Communicate the result back to the main thread
        ; GHC.liftIO $ putMVar resultMV result
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

-- FIXME: temporary hack
logRef :: IORef [String]
{-# NOINLINE logRef #-}
logRef = unsafePerformIO $ newIORef []

-- logAction :: GHC.LogAction
logAction dflags severity srcSpan _style msg
  -- = modifyIORef logRef (GHC.showSDoc dflags (GHC.pprLocErrMsg msg) :)
  = modifyIORef logRef (GHC.showSDoc dflags (GHC.mkLocMessage severity srcSpan msg) :)


-- Runtime system support
-- ----------------------
foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case
