-- | Re-export all symbols and instances of the process-extras package.
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module System.Process.Extras
    ( ListLikeProcessIO(readCreateProcessWithExitCode)
    , readProcessWithExitCode
    , showCreateProcessForUser
    , showCmdSpecForUser
    ) where

import Data.Text (pack, unpack)
import System.Process (CmdSpec(..), CreateProcess(..), showCommandForUser)
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()
import System.Process.ListLike (ListLikeProcessIO(readCreateProcessWithExitCode), readProcessWithExitCode)
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
instance ListLikeProcessIO String where
  readCreateProcessWithExitCode p input = do
    (code, out, err) <- readCreateProcessWithExitCode p (pack input)
    return (code, unpack out, unpack err)
