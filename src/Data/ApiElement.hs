{-# LANGUAGE OverloadedStrings #-}

module Data.ApiElement (isASchema, jsonSchema) where

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
  fromMaybe False $ fmap (arrayElem (String "messageBodySchema")) $ join $ objectLookup "classes"
                                                                           <$> objectLookup "meta" v

jsonSchema :: Value -> Maybe Schema
jsonSchema v =
  join $ fmap (either (\_ -> Nothing) (\v -> Just v)) $ fmap decodeJson $ fmap fromStrict $ fmap
                                                                                              encodeUtf8 $ join $ asText
                                                                                                                  <$> objectLookup
                                                                                                                        "content"
                                                                                                                        v

objectLookup :: Text -> Value -> Maybe Value
objectLookup k (Object m) = k `HMS.lookup` m
objectLookup _ _ = Nothing

arrayElem :: Value -> Value -> Bool
arrayElem k (Array a) = k `V.elem` a
arrayElem _ _ = False

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing
