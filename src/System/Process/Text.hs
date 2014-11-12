{-# LANGUAGE FlexibleContexts #-}
module System.Process.Text where

import Data.Text (Text)
import System.Process
import System.Process.ListLike.Classes (ProcessOutput)
import qualified System.Process.ListLike.Read as LL (readCreateProcess, readProcess)
import System.Exit (ExitCode)

readCreateProcess :: ProcessOutput Text b => CreateProcess -> Text -> IO b
readCreateProcess = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but takes a
-- CreateProcess instead of a command and argument list, and reads and
-- writes type 'ByteString'
readCreateProcessWithExitCode
    :: CreateProcess            -- ^ command to run
    -> Text                     -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readProcessWithExitCode :: FilePath -> [String] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Like 'System.Process.readProcess', but using 'Text'
readProcess :: FilePath -> [String] -> Text -> IO Text
readProcess = LL.readProcess
