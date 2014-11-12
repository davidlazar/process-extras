{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module System.Process.ListLike.StrictString where

import Control.DeepSeq (force)
import Data.ListLike (hGetContents)
import System.Process.ListLike.Classes (ListLikeLazyIO(readChunks))

instance ListLikeLazyIO String Char where
  readChunks h = hGetContents h >>= return . force . (: [])
