{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
      waitOut <- forkWait $ T.hGetContents outh

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ T.hGetContents errh

      -- now write and flush any input
      unless (T.null input) $ do T.hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- waitForProcess pid

      return (ex, out, err)
