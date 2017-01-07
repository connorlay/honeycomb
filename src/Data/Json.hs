module Data.Json (decodeJson) where

import           Data.Aeson                    (FromJSON, Value, eitherDecode)
import           Data.ByteString.Lazy          (ByteString)
import           Data.JsonSchema.Draft4.Schema (Schema)

decodeJson :: (FromJSON a) => ByteString -> Either String a
decodeJson =
  eitherDecode
