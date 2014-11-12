{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module System.Process.ListLike.LazyString where

import Data.Text as T (unpack)
import Data.Text.Lazy as LT (toChunks)
import System.Process.ListLike.Classes (ListLikeLazyIO(readChunks))
import System.Process.ListLike.Instances ()

-- | This String instance is implemented using the Lazy Text instance.
-- Otherwise (without some serious coding) String would be a strict
-- instance .  Note that the 'System.Process.readProcess' in the
-- process library is strict, while our equivalent is not - see test4
-- in Tests/Dots.hs.
instance ListLikeLazyIO String Char where
  readChunks h = readChunks h >>= return . map T.unpack . concat . map LT.toChunks
