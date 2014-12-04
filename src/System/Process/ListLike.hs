-- | Re-export all symbols and instances of the process-extras package.
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module System.Process.ListLike
    ( ListLikeProcessIO(forceOutput)
    , readCreateProcessWithExitCode
    , readProcessWithExitCode
    , showCreateProcessForUser
    , showCmdSpecForUser
    ) where

import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
-- import Data.Text (pack, unpack)
import System.Process (CmdSpec(..), CreateProcess(..), showCommandForUser)
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()
import System.Process.Common (ListLikeProcessIO(forceOutput), readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Process.Text ()
import System.Process.Text.Lazy ()

-- | System.Process utility functions.
showCreateProcessForUser :: CreateProcess -> String
showCreateProcessForUser p =
    showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> " (in " ++ d ++ ")") (cwd p)

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args

-- | Like 'System.Process.readProcessWithExitCode' that takes a 'CreateProcess'.
instance ListLikeProcessIO String Char where
    -- This is required because strings are magically lazy.  Without it
    -- processes get exit status 13 - file read failures.
    forceOutput = C.evaluate . force
{-
  readCreateProcessWithExitCode p input = do
    (code, out, err) <- readCreateProcessWithExitCode p (pack input)
    return (code, unpack out, unpack err)
-}
