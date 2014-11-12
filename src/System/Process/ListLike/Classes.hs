{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module System.Process.ListLike.Classes
    ( ListLikeLazyIO(..)
    , ProcessOutput(..)
    ) where

import Control.Exception (SomeException)
import Data.ListLike (ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid)
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode)
import System.IO hiding (hPutStr, hGetContents)
import System.Process (ProcessHandle)

-- | Methods for turning the output of a process into a monoid.
class Monoid b => ProcessOutput a b | b -> a where
    pidf :: ProcessHandle -> b
    outf :: a -> b
    errf :: a -> b
    intf :: SomeException -> b
    codef :: ExitCode -> b

-- | Class of types which can be used as the input and outputs of
-- these process functions.
class ListLikeIO a c => ListLikeLazyIO a c where
  readChunks :: Handle -> IO [a]
  -- ^ Read the list of chunks from this handle.  For lazy types this
  -- is just a call to 'hGetContents' followed by 'toChunks'.  For strict
  -- types it might return a singleton list.  Strings are trickier.
