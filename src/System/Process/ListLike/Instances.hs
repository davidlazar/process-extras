{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}

-- | ListLikeLazyIO instances for strict and lazy types.  If you start a
-- long running process with a strict type it will block until the
-- process finishes.  Why not try a lazy type?

{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ListLike.Instances where

import Control.DeepSeq (force, NFData)
import Control.Exception as E (evaluate, throw)
import Data.ByteString.Char8 as B (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ListLike.IO (hGetContents)
import Data.Monoid (mempty)
import Data.Text as T (Text)
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)
import System.Exit (ExitCode)
import System.Process.ListLike.Classes (ListLikeLazyIO(..), ProcessOutput(..))

instance ListLikeLazyIO B.ByteString Word8 where
  --setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f = maybe (return ()) (\ h -> hSetBinaryMode h True)
  readChunks h = hGetContents h >>= return . force . (: [])

instance ListLikeLazyIO T.Text Char where
  --setModes _ _  = return ()
  readChunks h = hGetContents h >>= return . force . (: [])

instance ListLikeLazyIO L.ByteString Word8 where
  --setModes _ (inh, outh, errh, _) = f inh >> f outh >> f errh where f = maybe (return ()) (\ h -> hSetBinaryMode h True)
  readChunks h = hGetContents h >>= evaluate . Prelude.map (L.fromChunks . (: [])) . L.toChunks

instance ListLikeLazyIO LT.Text Char where
  --setModes _ _  = return ()
  readChunks h = hGetContents h >>= evaluate . Prelude.map (LT.fromChunks . (: [])) . LT.toChunks

instance ListLikeLazyIO a c => ProcessOutput a (ExitCode, a, a) where
    pidf _ = mempty
    codef c = (c, mempty, mempty)
    outf x = (mempty, x, mempty)
    errf x = (mempty, mempty, x)
    intf e = throw e

-- | This lets us use DeepSeq's 'Control.DeepSeq.force' on a stream
-- of Chunks.
instance NFData ExitCode
