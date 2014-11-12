#!/usr/bin/runhaskell

import Control.Exception (throw)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (userError)

main =
    do let e = userError "This is a GHC.IO.Exception.UserError"
       hPutStrLn stderr "Here comes an exception"
       throw e
