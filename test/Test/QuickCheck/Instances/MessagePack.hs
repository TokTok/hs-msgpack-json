{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData #-}
module Test.QuickCheck.Instances.MessagePack () where

import           Control.Arrow                        (first)
import           Data.List                            (nubBy, sortOn)
import           Data.MessagePack.Types               (Object (..))
import qualified Data.Vector                          as V
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary,
                                                       genericShrink, shrink)
import qualified Test.QuickCheck.Gen                  as Gen
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Text       ()
import           Test.QuickCheck.Instances.Vector     ()


instance Arbitrary Object where
    arbitrary = Gen.sized $ \n -> Gen.oneof
        [ pure ObjectNil
        , ObjectBool   <$> arbitrary
        , ObjectInt    <$> negatives
        , ObjectWord   <$> arbitrary
        -- TODO(iphydf): Conversion goes back to Double. Should we make floats?
        -- , ObjectFloat . mustFloat <$> arbitrary
        , ObjectDouble . mustFloat <$> arbitrary
        , ObjectStr    <$> arbitrary
        -- TODO(iphydf): Support bin?
        -- , ObjectBin    <$> (BS.pack <$> arbitrary)
        , ObjectArray  <$> Gen.resize (n `div` 2) arbitrary
        , ObjectMap . sortUniqKeys . V.map (first ObjectStr) <$> Gen.resize (n `div` 4) arbitrary
        -- TODO(iphydf): Support ext?
        -- , ObjectExt    <$> arbitrary <*> arbitrary
        ]
      where
        negatives = Gen.choose (minBound, -1)
        -- Ensure that floats must always be floats, otherwise a round-trip
        -- through json will turn them into ints.
        mustFloat :: (Eq f, Floating f, RealFrac f) => f -> f
        mustFloat x
            | isFloat x = x + 0.5
            | otherwise = x

    shrink = map makeValid . filter isValid . genericShrink
      where
        isValid :: Object -> Bool
        isValid (ObjectInt n)    = n < 0
        isValid (ObjectDouble f) = not $ isFloat f
        isValid ObjectFloat{}    = False
        isValid ObjectBin{}      = False
        isValid ObjectExt{}      = False
        isValid _                = True

        makeValid (ObjectMap kv) = ObjectMap . sortUniqKeys $ kv
        makeValid x = x

isFloat :: (Eq f, Floating f, RealFrac f) => f -> Bool
isFloat x = fromInteger (floor x) == x

sortUniqKeys :: V.Vector (Object, Object) -> V.Vector (Object, Object)
sortUniqKeys = V.fromList . nubBy (\a b -> fst a == fst b) . sortOn fst . V.toList
