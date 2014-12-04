{-# LANGUAGE MultiParamTypeClasses #-}
module System.Process.Common where

import Control.Exception
import Control.Monad
import Data.ListLike (null)
import Data.ListLike.IO (ListLikeIO, hGetContents, hPutStr)
import Prelude hiding (null)
import System.Exit (ExitCode)
import System.IO (hClose, hFlush)
import System.Process
import Utils (forkWait)

class ListLikeIO a c => ListLikeProcessIO a c where
    forceOutput :: a -> IO a

-- | Like 'System.Process.readProcessWithExitCode', but with generalized input and output type.
readProcessWithExitCode
    :: ListLikeProcessIO a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

readCreateProcessWithExitCode :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess p{ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    flip onException
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ hGetContents outh >>= forceOutput

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ hGetContents errh >>= forceOutput

      -- now write and flush any input
      unless (null input) $ do hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
