{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.MessagePack.AesonSpec where

import           Control.Arrow                         (left)
import           Control.Monad.Validate                (Validate, runValidate)
import           Data.MessagePack.Aeson                (fromAeson, toAeson)
import           Data.MessagePack.Types                (DecodeError,
                                                        Object (..),
                                                        errorMessages)
import qualified Data.Vector                           as V
import           Test.Hspec                            (Spec, describe, it,
                                                        shouldBe)
import           Test.QuickCheck                       (property)
import           Test.QuickCheck.Instances.MessagePack ()

resultOf :: Validate DecodeError a -> Either [String] a
resultOf = left errorMessages . runValidate

spec :: Spec
spec =
    describe "Aeson conversion" $ do
        it "errors on a non-string-keyed map" $
            resultOf (toAeson (ObjectMap (V.fromList [(ObjectNil,ObjectWord 2)])))
            `shouldBe` Left ["invalid encoding for Text"]

        it "can round-trip any arbitrary msgpack object" $
            property $ \(x :: Object) ->
                resultOf (toAeson x >>= fromAeson) `shouldBe` Right x
