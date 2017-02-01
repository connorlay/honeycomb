{-# LANGUAGE OverloadedStrings #-}

module JsonSchema.References where

import           Search.DepthFirstSearch
import           Data.JsonSchema.Draft4.Schema (Schema (..))
import           Data.Validator.Draft4.Any     (TypeValidator (..))
import           Data.Validator.Draft4.Array   (Items (..))
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.HashMap.Lazy             (HashMap (..), empty, toList)

instance Expandable Schema where
  expand schema =
    case _schemaType schema of
      Just (TypeValidatorString "object") ->
        map snd . toList . fromMaybe empty . _schemaProperties $ schema

      Just (TypeValidatorString "array") ->
        maybeToList . toSubschema $ schema

      _ ->
        []

toSubschema :: Schema -> Maybe Schema
toSubschema schema =
  case _schemaItems schema of
    Just (ItemsObject subschema) ->
      Just subschema

    _ ->
      Nothing

collectRefs :: Schema -> [String]
collectRefs schema =
  traverseAst schema isARef
  where
    isARef :: Schema -> Bool
    isARef = isJust . _schemaRef
