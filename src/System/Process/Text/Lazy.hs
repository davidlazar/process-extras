{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text.Lazy where

import Control.DeepSeq (rnf)
import Control.Exception
import qualified Control.Exception as C (evaluate)
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Process
import System.Process.ListLike
import System.IO
import Utils (forkWait)

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
instance ListLikeProcessIO Text where
  readCreateProcessWithExitCode p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess p{ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    flip onException
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      out <- T.hGetContents outh
      waitOut <- forkWait $ C.evaluate $ rnf out

      -- fork off a thread to start consuming stderr
      err <- T.hGetContents errh
      waitErr <- forkWait $ C.evaluate $ rnf err

      -- now write and flush any input
      unless (T.null input) $ do T.hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      waitOut
      waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
