module System.Process.Common where

import System.Exit (ExitCode)
import System.Process (CreateProcess, proc)

class ListLikeProcessIO a where
    readCreateProcessWithExitCode
        :: CreateProcess            -- ^ command and arguments to run
        -> a                        -- ^ standard input
        -> IO (ExitCode, a, a)      -- ^ exitcode, stdout, stderr

-- | Like 'System.Process.readProcessWithExitCode', but with generalized input and output type.
readProcessWithExitCode
    :: ListLikeProcessIO a =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input
