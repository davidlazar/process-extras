{-# LANGUAGE FlexibleContexts #-}
module System.Process.String where

import System.Exit (ExitCode)
import System.Process (CreateProcess(..))
import System.Process.ListLike.Classes (ProcessOutput, ListLikeLazyIO)
import System.Process.ListLike.Read as LL (readCreateProcess)

-- No ListLikeLazyIO instance is imported here because we have two and
-- naturally they conflict.
readCreateProcess :: (ListLikeLazyIO String Char, ProcessOutput String b) => CreateProcess -> String -> IO b
readCreateProcess = LL.readCreateProcess

readCreateProcessWithExitCode
    :: ListLikeLazyIO String Char =>
       CreateProcess                 -- ^ command to run
    -> String                        -- ^ standard input
    -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = LL.readCreateProcess
