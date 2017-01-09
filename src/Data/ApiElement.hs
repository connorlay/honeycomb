{-# LANGUAGE OverloadedStrings #-}

module Data.ApiElement (isASchema, asJsonSchema) where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Either                   (either)
import qualified Data.HashMap.Strict           as HMS (HashMap, fromList,
                                                       lookup)
import           Data.Json                     (decodeJson)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Data.Maybe                    (fromMaybe)
import           Data.Text
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Vector                   as V (elem, fromList)

{- TODO: refactor with Lenses? -}
isASchema :: Value -> Bool
isASchema v =
  maybe False (arrayElem $ String "messageBodySchema")
    (objectLookup "classes" =<< objectLookup "meta" v)

asJsonSchema :: Value -> Maybe Schema
asJsonSchema v =
  either (\_ -> Nothing) (\v -> Just v)
  <$> decodeJson
  =<< fromStrict
      <$> encodeUtf8
      <$> (asText =<< objectLookup "content" v)

objectLookup :: Text -> Value -> Maybe Value
objectLookup k (Object m) = k `HMS.lookup` m
objectLookup _ _ = Nothing

arrayElem :: Value -> Value -> Bool
arrayElem k (Array a) = k `V.elem` a
arrayElem _ _ = False

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing
