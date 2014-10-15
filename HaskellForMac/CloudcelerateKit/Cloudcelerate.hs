{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- |
-- Module    : Cloudcelerate
-- Copyright : [2014] Manuel M T Chakravarty
-- License   : All rights reserved
--
-- Maintainer: Manuel M T Chakravarty <chak@justtesting.org>
--
-- Cloudcelerate API

module Cloudcelerate () where

  -- standard libraries
import Control.Concurrent
import Control.Exception (SomeException, mask, try, throwIO)
import qualified Control.Exception as Exc
import Control.DeepSeq (rnf)
import Control.Monad
import Data.List
import Data.String
import Data.Word
import Foreign.C.Error
import Foreign.Ptr
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Posix.Signals

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC
  
  -- other libraries
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map  as Map
import Network.HTTP.Types  hiding (statusCode)
import Network.HTTP.Client (HttpException)
import Network.Wreq

objc_import ["<Foundation/Foundation.h>"]


sandboxURL = "http://api.sandbox.cloudcelerate.io/v1.0"

postUsersMac :: String -> String -> IO (Maybe String)
postUsersMac username storeReceiptPath
  = do
    { let opts = defaults & param "type   "  .~ ["mac"]
                          & param "username" .~ [fromString username]
    ; resp <- postWith opts (sandboxURL </> "users") (partFile "file" storeReceiptPath)
    ; return $ if resp^.responseStatus == created201
               then Just $ show $ resp^.responseBody.(key "api_key")._String
               else Nothing
    }
    `Exc.catch` (\(_e :: HttpException) -> return Nothing)      -- FIXME: we need to log the exception!
  
getPing :: Maybe String -> String -> IO Bool
getPing username apiKey
  = do
    { let opts = case username of 
                   Nothing       -> defaults
                   Just username -> defaults & auth .~ basicAuth (fromString username) (fromString apiKey)
    ; resp <- getWith opts (sandboxURL </> "ping")
    ; return $ resp^.responseStatus == ok200
    }
    `Exc.catch` (\(_e :: HttpException) -> return False)

postPrograms :: String -> String -> String -> IO (Maybe String)
postPrograms username apiKey projectPath
  = do
    { (exitCode, stdout, stderr) <- readProcessWithExitCode_atProjectPath "cabal" ["sdist"] ""
    ; if exitCode /= ExitSuccess
      then return $ Just (last . lines $ stderr)
      else do
    {   -- This is hacky, but it will do for now.
    ; case words . last . lines $ stdout of
        ["Source", "tarball", "created:", sdistName] -> doUpload (projectPath </> sdistName)
        result                                       -> return $ Just ("Could not create project archive ("
                                                                       ++ unwords result ++ ").")
    } }
  where
    doUpload sdistPath
      = do
        { let opts = defaults & auth .~ basicAuth (fromString username) (fromString apiKey)
        ; resp <- postWith opts (sandboxURL </> "programs") (partFile "file" sdistPath)
        ; return $ if resp^.responseStatus == created201
                   then Nothing 
                   else (Just "Unable to upload project.")
        }
        `Exc.catch` (\(e :: HttpException) -> return $ Just ("Internal error: " ++ show e))
    --
    -- NB: The following was copied from System.Process to be able to set the CWD.
    readProcessWithExitCode_atProjectPath cmd args input = do
        let cp_opts = (proc cmd args) {
                        std_in  = CreatePipe,
                        std_out = CreatePipe,
                        std_err = CreatePipe,
                        cwd     = Just projectPath
                      }
        withCreateProcess_ "readProcessWithExitCode" cp_opts $
          \(Just inh) (Just outh) (Just errh) ph -> do
    
            out <- hGetContents outh
            err <- hGetContents errh
    
            -- fork off threads to start consuming stdout & stderr
            withForkWait  (Exc.evaluate $ rnf out) $ \waitOut ->
             withForkWait (Exc.evaluate $ rnf err) $ \waitErr -> do
    
              -- now write any input
              unless (null input) $
                ignoreSigPipe $ hPutStr inh input
              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
              ignoreSigPipe $ hClose inh
    
              -- wait on the output
              waitOut
              waitErr
    
              hClose outh
              hClose errh
    
            -- wait on the process
            ex <- waitForProcess ph
    
            return (ex, out, err)
    --
    withCreateProcess_
      :: String
      -> CreateProcess
      -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
      -> IO a
    withCreateProcess_ fun c action =
        Exc.bracketOnError (createProcess c) cleanupProcess
                           (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
    --
    cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
                   -> IO ()
    cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
        terminateProcess ph
        -- Note, it's important that other threads that might be reading/writing
        -- these handles also get killed off, since otherwise they might be holding
        -- the handle lock and prevent us from closing, leading to deadlock.
        maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
        maybe (return ()) hClose mb_stdout
        maybe (return ()) hClose mb_stderr
        -- terminateProcess does not guarantee that it terminates the process.
        -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
        -- that it stops. If it doesn't stop, we don't want to hang, so we wait
        -- asynchronously using forkIO.
        _ <- forkIO (waitForProcess ph >> return ())
        return ()
    --
    withForkWait :: IO () -> (IO () ->  IO a) -> IO a
    withForkWait async body = do
      waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
      mask $ \restore -> do
        tid <- forkIO $ try (restore async) >>= putMVar waitVar
        let wait = takeMVar waitVar >>= either throwIO return
        restore (body wait) `Exc.onException` killThread tid
    --
    ignoreSigPipe :: IO () -> IO ()
    ignoreSigPipe = Exc.handle $ \e -> case e of
                                         IOError { ioe_type  = ResourceVanished
                                                 , ioe_errno = Just ioe }
                                           | Errno ioe == ePIPE -> return ()
                                         _ -> throwIO e

postJobs :: String -> String -> String -> Maybe String -> IO (Maybe String)
postJobs username apiKey programName dataName
  = do
    { let optsBase  = defaults & auth            .~ basicAuth (fromString username) (fromString apiKey)
                               & param "program" .~ [fromString programName]
          optsFinal = case dataName of
                        Nothing       -> optsBase
                        Just dataName -> optsBase & param "dataset" .~ [fromString dataName]
    ; resp <- postWith optsFinal (sandboxURL </> "jobs") ([] :: [Part])
    ; return $ if resp^.responseStatus == created201
               then Nothing 
               else (Just "Failed to initiate compute job.")
    }
   `Exc.catch` (\(e :: HttpException) -> return $ Just ("Internal error: " ++ show e))


objc_interface [cunit|

// Indirection as the dynlib containing the Haskell code is not directly visible to HfM.
void CloudcelerateKit_initialise(void);

@interface Cloudcelerate : NSObject

/// Request the API key for the given username-storeReceipt combo unless the username was already used with a different receipt.
///
+ (typename NSString *)newMASAccount:(typename NSString *)userName storeReceiptPath:(typename NSString *)storeReceiptPath;

/// Validate a given username-apikey combo.
///
+ (typename BOOL)validateUsername:(typename NSString *)username apiKey:(typename NSString *)apiKey;

/// Upload the Cabal sdist of the given project at the given location to Cloudcelerate.
///
/// The Cloudcelerate name of the project is the directory name.
///
/// If the upload fails, return an error string; otherwise, return 'nil'.
///
+ (typename NSString *)uploadProgramFor:(typename NSString *)username 
                                 apiKey:(typename NSString *)apiKey
                                fileURL:(typename NSURL    *)fileURL;

/// Issue a job running the given program in the given dataset.
///
/// If the *issuing* fails, return an error string; otherwise, return 'nil'. 
///
/// NB: Jobs are executed asynchoniously. Even if this call succeeds, the job may still fail.
///
+ (typename NSString *)runJobFor:(typename NSString *)username
                          apiKey:(typename NSString *)apiKey
                     programName:(typename NSString *)programName
                        dataName:(typename NSString *)dataName;

@end
|]


objc_implementation [Typed 'postUsersMac, Typed 'getPing, Typed 'postPrograms, Typed 'postJobs] [cunit|


void Cloudcelerate_initialise(void);
void CloudcelerateKit_initialise()
{
  Cloudcelerate_initialise();
}

@implementation Cloudcelerate

+ (typename NSString *)newMASAccount:(typename NSString *)username storeReceiptPath:(typename NSString *)storeReceiptPath
{
  return postUsersMac(username, storeReceiptPath);
}

+ (typename BOOL)validateUsername:(typename NSString *)username apiKey:(typename NSString *)apiKey
{
  return getPing(username, apiKey);
}

+ (typename NSString *)uploadProgramFor:(typename NSString *)username 
                                 apiKey:(typename NSString *)apiKey
                                fileURL:(typename NSURL    *)fileURL
{
  return postPrograms(username, apiKey, [fileURL path]); 
}

+ (typename NSString *)runJobFor:(typename NSString *)username
                          apiKey:(typename NSString *)apiKey
                     programName:(typename NSString *)programName
                        dataName:(typename NSString *)dataName;
{
  return postJobs(username, apiKey, programName, dataName);
}

@end
|]


objc_emit

foreign export ccall "Cloudcelerate_initialise" objc_initialise :: IO ()
