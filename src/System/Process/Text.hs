{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text where

import Control.Applicative ((<$>))
import Control.Monad
import Data.ListLike.IO (hGetContents)
import Data.Text (Text)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
instance ListLikeProcessIO Text Char where
    forceOutput = return
    readChunks h = (: []) <$> hGetContents h

-- | Specialized version for backwards compatibility.
readProcessWithExitCode :: FilePath -> [String] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode :: CreateProcess -> Text -> IO (ExitCode, Text, Text)
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
