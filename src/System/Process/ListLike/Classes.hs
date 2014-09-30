{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module System.Process.ListLike.Classes
    ( ListLikeLazyIO(..)
    , ProcessOutput(..)
    ) where

import Control.Exception (SomeException)
import Data.ListLike (ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Monoid (Monoid(mempty, mappend))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitFailure))
import System.IO hiding (hPutStr, hGetContents)
import System.Process (ProcessHandle)

-- | Methods for turning the output of a process into a monoid.
class Monoid b => ProcessOutput a b | b -> a where
    pidf :: ProcessHandle -> b
    outf :: a -> b
    errf :: a -> b
    intf :: SomeException -> b
    codef :: ExitCode -> b

-- | A process usually has one 'ExitCode' at the end of its output, this 'Monoid'
-- instance lets us build the type returned by 'System.Process.readProcessWithExitCode'.
instance Monoid ExitCode where
    mempty = ExitFailure 0
    mappend x (ExitFailure 0) = x
    mappend _ x = x

-- | Class of types which can be used as the input and outputs of
-- these process functions.
class ListLikeIO a c => ListLikeLazyIO a c where
  readChunks :: Handle -> IO [a]
  -- ^ Read the list of chunks from this handle.  For lazy types this
  -- is just a call to 'hGetContents' followed by 'toChunks'.  For strict
  -- types it might return a singleton list.  Strings are trickier.
