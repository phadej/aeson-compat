{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Prelude                   ()
import Prelude.Compat

import Data.Version              (Version (..))
import Test.Tasty.QuickCheck

import Data.Orphans              ()
import Test.QuickCheck.Instances ()

#if !MIN_VERSION_quickcheck_instances(0,3,12)
import Data.Vector               as V
#endif

#if !MIN_VERSION_QuickCheck(2,8,3)
import Control.Applicative       (Const (..))
import Data.List.NonEmpty        (NonEmpty (..))
import Data.Proxy                (Proxy (..))
import Data.Tagged               (Tagged (..))
#endif

#if !(MIN_VERSION_QuickCheck(2,8,0) && MIN_VERSION_base(4,8,0))
import Numeric.Natural           (Natural)
#endif

#if !MIN_VERSION_quickcheck_instances(0,3,12)
instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink    =  fmap V.fromList . shrink . V.toList
#endif

#if !(MIN_VERSION_QuickCheck(2,8,0) && MIN_VERSION_base(4,8,0))
instance Arbitrary Natural where
  arbitrary = fmap (fromInteger . abs) arbitrary
#endif

instance Arbitrary Version where
  arbitrary = do
    x <- fmap abs arbitrary
    xs <- (fmap . fmap) abs arbitrary
    return $ Version (x : xs) []

#if !MIN_VERSION_QuickCheck(2,8,3)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    return (x :| xs)

instance Arbitrary a => Arbitrary (Tagged t a) where
  arbitrary = fmap Tagged arbitrary

instance Arbitrary a => Arbitrary (Const a b) where
  arbitrary = fmap Const arbitrary

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy
#endif
