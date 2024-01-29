{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}

-- | Aeson bridge for MessagePack
module Data.MessagePack.Aeson (
  -- * Conversion functions
  toAeson,
  fromAeson,
  viaFromJSON,
) where

import           Control.Monad.Validate (MonadValidate, refute)
import qualified Data.Aeson             as A
import qualified Data.Aeson.Key         as K
import qualified Data.Aeson.KeyMap      as KM
import           Data.Int               (Int64)
import           Data.MessagePack.Types as MP
import           Data.Scientific        (floatingOrInteger)
import           Data.String            (fromString)
import qualified Data.Vector            as V
import           Data.Word              (Word64)

-- | Convert 'MP.Object' to JSON 'A.Value'
toAeson :: MonadValidate MP.DecodeError m => MP.Object -> m A.Value
toAeson = \case
  ObjectNil      -> pure A.Null
  ObjectBool b   -> pure . A.Bool $ b
  ObjectInt n    -> pure . A.Number . fromIntegral $ n
  ObjectWord n   -> pure . A.Number . fromIntegral $ n
  ObjectFloat f  -> pure . A.Number . realToFrac $ f
  ObjectDouble d -> pure . A.Number . realToFrac $ d
  ObjectStr t    -> pure . A.String $ t
  ObjectBin _    -> refute "ObjectBin is not supported by JSON"
  ObjectArray v  -> A.Array <$> V.mapM toAeson v
  ObjectMap m    ->
    A.Object . KM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> (K.fromText <$> from k) <*> toAeson v) m
      where from = MP.fromObjectWith MP.defaultConfig
  ObjectExt _ _  -> refute "ObjectExt is not supported by JSON"

isWord64 :: Integer -> Bool
isWord64 n = n >= fromIntegral (minBound :: Word64) && n <= fromIntegral (maxBound :: Word64)

isInt64 :: Integer -> Bool
isInt64 n = n >= fromIntegral (minBound :: Int64) && n <= fromIntegral (maxBound :: Int64)

-- | Convert JSON 'A.Value' to 'MP.Object'
fromAeson :: MonadValidate MP.DecodeError m => A.Value -> m MP.Object
fromAeson = \case
  A.Null      -> pure ObjectNil
  A.Bool b    -> pure . ObjectBool $ b
  A.Number s  ->
    -- NOTE floatingOrInteger can OOM on untrusted input
    case floatingOrInteger s of
      Left  f -> pure . ObjectDouble $ f
      Right i
        | isWord64 i -> pure . ObjectWord . fromInteger $ i
        | isInt64  i -> pure . ObjectInt  . fromInteger $ i
        | otherwise  -> refute "number out of bounds"
  A.String t  -> pure . ObjectStr $ t
  A.Array v   -> ObjectArray <$> traverse fromAeson v
  A.Object o  -> ObjectMap . V.fromList <$> traverse fromEntry (KM.toList o)
    where
      fromEntry (k, v) = (ObjectStr (K.toText k),) <$> fromAeson v

-- | Helpers to piggyback off a JSON encoder / decoder when creating a MessagePack
-- instance.
--
-- Not as efficient as a direct encoder.
viaFromJSON :: (MonadValidate MP.DecodeError m, A.FromJSON a) => MP.Object -> m a
viaFromJSON o = do
  v <- toAeson o
  case A.fromJSON v of
    A.Success a -> return a
    A.Error   e -> refute (fromString e)
