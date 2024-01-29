{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.MessagePack.AesonSpec where

import           Control.Monad.Validate                (runValidate)
import           Data.MessagePack.Aeson                (fromAeson, toAeson)
import           Data.MessagePack.Types                (Object (..))
import qualified Data.Vector                           as V
import           Test.Hspec                            (Spec, describe, it,
                                                        shouldBe)
import           Test.QuickCheck                       (property)
import           Test.QuickCheck.Instances.MessagePack ()

spec :: Spec
spec =
    describe "Aeson conversion" $ do
        it "errors on a non-string-keyed map" $
            runValidate (toAeson (ObjectMap (V.fromList [(ObjectNil,ObjectWord 2)]))) `shouldBe` Left "invalid encoding for Text"

        it "can round-trip any arbitrary msgpack object" $
            property $ \(x :: Object) ->
                runValidate (toAeson x >>= fromAeson) `shouldBe` Right x
