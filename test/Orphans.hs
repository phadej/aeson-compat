{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import Data.Vector as V
import Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink    =  fmap V.fromList . shrink . V.toList
