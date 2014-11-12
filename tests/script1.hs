#!/usr/bin/runhaskell

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

main =
    do hPutStrLn stderr "This is an error message."
       exitWith (ExitFailure 123)
