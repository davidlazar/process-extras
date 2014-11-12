{-# LANGUAGE FlexibleContexts #-}
module System.Process.Text.Lazy where

import Data.Text.Lazy (Text)
import System.Exit (ExitCode)
import System.Process
import System.Process.ListLike.Classes (ProcessOutput)
import qualified System.Process.ListLike.Read as LL (readCreateProcess, readProcess)

readCreateProcess :: ProcessOutput Text b => CreateProcess -> Text -> IO b
readCreateProcess = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
readCreateProcessWithExitCode
    :: CreateProcess             -- ^ command to run
    -> Text                      -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readProcessWithExitCode :: FilePath -> [String] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Like 'System.Process.readProcess', but using 'Text'
readProcess :: FilePath -> [String] -> Text -> IO Text
readProcess = LL.readProcess
