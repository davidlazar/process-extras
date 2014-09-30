{-# LANGUAGE FlexibleContexts #-}
module System.Process.ByteString.Lazy where

import Data.ByteString.Lazy (ByteString)
import System.Exit (ExitCode)
import System.Process
import qualified System.Process.ListLike.Read as LL (readCreateProcess, readProcess)
import System.Process.ListLike.Classes (ProcessOutput)

readCreateProcess :: ProcessOutput ByteString b => CreateProcess -> ByteString -> IO b
readCreateProcess = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but takes a
-- CreateProcess instead of a command and argument list, and reads and
-- writes type 'ByteString'
readCreateProcessWithExitCode
    :: CreateProcess            -- ^ command to run
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = LL.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readProcessWithExitCode :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Like 'System.Process.readProcess', but using 'ByteString'
readProcess :: FilePath -> [String] -> ByteString -> IO ByteString
readProcess = LL.readProcess
