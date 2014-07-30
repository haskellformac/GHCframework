{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : PrintInterceptor
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
--
-- Redirects data GHC emits with the "interactive print" hook, to allow it to be captured by HfM.

module PrintInterceptor (
  interactivePrintChan,
  interactivePrintInterceptor
) where

  -- standard library
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.DeepSeq
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe               (unsafePerformIO)


-- |Global channel to pipe though data from the interactive print hook.
--
-- This will always return the identical value, even if the CAF is reevaluated.
--
interactivePrintChan :: TChan String
{-# NOINLINE interactivePrintChan #-}
interactivePrintChan = unsafePerformIO $ do
    { stableChan <- peek stableChanPtr
    ; if castStablePtrToPtr stableChan == nullPtr       -- DATA segment will be initialised to 0s
      then do   -- first call => initialise
      { chan <- newTChanIO
      ; newStableChan <- newStablePtr chan
      ; poke stableChanPtr newStableChan
      ; return chan
      }
      else do   -- subsequent call => return previously allocated channel
      { deRefStablePtr stableChan
      }
    }

-- |Channel source to be put into GHC's "interactive print" hook
--
interactivePrintInterceptor :: Show a => a -> IO ()
interactivePrintInterceptor v 
  = let str = show v
    in
    str `deepseq` (atomically $ writeTChan interactivePrintChan str)

foreign import ccall "&stableChanPtr" stableChanPtr :: Ptr (StablePtr (TChan String))
