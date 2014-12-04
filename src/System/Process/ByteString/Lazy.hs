{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString.Lazy where

import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
instance ListLikeProcessIO ByteString Word8 where
    forceOutput = C.evaluate . force

-- | Specialized version for backwards compatibility.
readProcessWithExitCode :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode :: CreateProcess -> ByteString -> IO (ExitCode, ByteString, ByteString)
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
