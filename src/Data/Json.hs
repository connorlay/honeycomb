module Data.Json (decodeJson) where

import           Data.Aeson           (Value, eitherDecode)
import           Data.ByteString.Lazy (ByteString)

decodeJson :: ByteString -> Either String Value
decodeJson =
  eitherDecode
