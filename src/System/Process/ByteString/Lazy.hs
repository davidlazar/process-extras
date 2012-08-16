module System.Process.ByteString.Lazy where

import Control.Exception
import qualified Control.Exception as C (evaluate)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import System.Process
import System.Exit (ExitCode)
import System.IO
import Utils (forkWait)

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    flip onException
      (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- B.hGetContents outh
      waitOut <- forkWait $ void $ C.evaluate $ B.length out

      -- fork off a thread to start consuming stderr
      err <- B.hGetContents errh
      waitErr <- forkWait $ void $ C.evaluate $ B.length err

      -- now write and flush any input
      unless (B.null input) $ do B.hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      waitOut
      waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
