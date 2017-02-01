{-# LANGUAGE OverloadedStrings #-}

module JsonSchema.References where

import           Search.DepthFirstSearch
import           Data.JsonSchema.Draft4.Schema (Schema (..))
import           Data.Validator.Draft4.Any     (TypeValidator (..))
import           Data.Validator.Draft4.Array   (Items (..))
import           Data.Validator.Reference
import           Data.Maybe                    (catMaybes, fromMaybe, maybeToList, isJust)
import           Data.HashMap.Lazy             (HashMap (..), empty, toList, fromList)
import           Data.Text                     (Text)
import           Data.List                     (nub)

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

type Ref = Text
type SchemaAndRefs = (Schema, [Ref])
type SchemaAndRefMap = (Schema, HashMap Ref Schema)

collectRefs :: Schema -> SchemaAndRefs
collectRefs schema =
  (schema, nub . catMaybes . map _schemaRef . traverseAst schema $ isARef)
  where
    isARef :: Schema -> Bool
    isARef = isJust . _schemaRef

resolveRefs :: SchemaAndRefs -> SchemaAndRefMap
resolveRefs (schema, refs) =
  (schema, fromList . catMaybes . map (resolveRef schema) $ refs)
    where
      resolveRef :: Schema -> Ref -> Maybe (Text, Schema)
      resolveRef schema ref =
        case resolveFragment (snd . resolveReference Nothing $ ref) schema of
          Just schema' -> Just (ref, schema')
          Nothing -> Nothing


