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
        ; _packageIds <- GHC.setSessionDynFlags (dflags {GHC.log_action =  logAction})
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
eval = error "eval is not implemented"

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
        ; GHC.setTargets [target]
        ; GHC.load GHC.LoadAllTargets
        ; msgs <- reverse <$> GHC.liftIO (readIORef logRef)
        ; GHC.liftIO $ writeIORef logRef []
        ; let result | null msgs = Result "Loaded module successfully."
                     | otherwise = Error $ concat msgs
        ; GHC.liftIO $ putMVar resultMV result
        }
    ; takeMVar resultMV
    }
  where
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
logAction dflags _severity _srcSpan _style msg
  -- = modifyIORef logRef (GHC.showSDoc dflags (GHC.pprLocErrMsg msg) :)
  = modifyIORef logRef (GHC.showSDoc dflags msg :)
