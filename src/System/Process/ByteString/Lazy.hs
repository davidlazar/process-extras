module System.Process.ByteString.Lazy where

import Control.Concurrent
import qualified Control.Exception as C
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import System.Process
import System.Exit (ExitCode)
import System.IO

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh
    _ <- forkIO $ C.evaluate (B.length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- B.hGetContents errh
    _ <- forkIO $ C.evaluate (B.length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (B.null input)) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)
