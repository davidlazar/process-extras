#!/usr/bin/runhaskell

import System.IO (hPutStrLn, stderr)

main =
    do c <- getChar
       hPutStrLn stderr ("Read one character: " ++ show c)
       putChar c
