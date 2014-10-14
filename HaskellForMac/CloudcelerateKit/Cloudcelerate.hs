{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving #-}

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
import Data.List
import Data.Word
import Foreign.Ptr
import System.FilePath

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC
  
  -- other libraries
-- import Data.Aeson
import Network.Curl

objc_import ["<Foundation/Foundation.h>"]


sandboxURL = "http://api.sandbox.cloudcelerate.io/v1.0"

postUsersMac :: String -> String -> IO (Maybe String)
postUsersMac username storeReceiptPath
  = withCurlDo $ do 
    { (code, response) <- curlGetString (sandboxURL </> "users" ++ "?type=mac&username=" ++ username)
                                        [ CurlFailOnError False
                                        , CurlHttpPost [HttpPost 
                                                        { postName     = "file"
                                                        , contentType  = Nothing
                                                        , content      = ContentFile storeReceiptPath
                                                        , extraHeaders = []
                                                        , showName     = Nothing
                                                        }]
                                        , CurlVerbose True
                                        ]
    ; if code == CurlOK && not ("error" `isInfixOf` response)
      then do
      { return $ Just (takeWhile (/= '"') . drop 15 . concat . lines $ response) -- FIXME: parse properly
      }
      else do
      { putStrLn $ "postUsersMac failed with '" ++ response ++ "'"
      ; return Nothing
      }
      -- Need to parse the error string and return it for reporting 
    }
    
getPing :: Maybe String -> String -> IO Bool
getPing username apiKey
  = withCurlDo $ do
    { (code, _) <- curlGetString (sandboxURL </> "ping") $
                                 credentials ++
                                 [ CurlFailOnError False
                                 , CurlVerbose True
                                 ]      
    ; return $ code == CurlOK
    }
  where
    credentials = case username of
                    Nothing       -> []
                    Just username -> [CurlHttpAuth [HttpAuthBasic], CurlUserName username, CurlUserPassword apiKey]

postPrograms :: String -> String -> String -> IO (Maybe String)
postPrograms username apiKey projectPath
  = return $ Just "not implemented"

postJobs :: String -> String -> String -> String -> IO (Maybe String)
postJobs username apiKey programName dataName
  = withCurlDo $ do 
    { (code, _) <- curlGetString (sandboxURL </> "jobs" ++ "?program=" ++ programName ++ "&dataset=" ++ dataName)
                                 [ CurlHttpAuth [HttpAuthBasic]
                                 , CurlUserName username
                                 , CurlUserPassword apiKey
                                 , CurlFailOnError False
                                 , CurlVerbose True
                                 ] 
    ; return $ if code == CurlOK then Nothing else (Just "Unable to start job")
                                                   -- FIXME: more detailed diagnostics
    }


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
