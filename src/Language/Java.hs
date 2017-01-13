{-# LANGUAGE OverloadedStrings #-}

module Language.Java (toJava) where

import Data.Validator.Draft4.Any (TypeValidator (..))
import Data.Validator.Draft4.Array (Items (..))
import Data.HashMap.Lazy (HashMap(..), empty, toList)
import Data.Text (Text(..), toTitle, unpack)
import Language.Java.Syntax
import Data.JsonSchema.Draft4.Schema (Schema (..))
import Text.Countable (singularize)
import Data.Maybe (fromMaybe, fromJust)

toJava :: Text -> Schema -> CompilationUnit
toJava name schema =
  CompilationUnit
    Nothing
    [] {- Imports go here -}
    [ ClassTypeDecl
        (ClassDecl
          [Public]
          (Ident . unpack $ name)
          []
          Nothing
          []
          (ClassBody . toClassMembers . fromMaybe empty . _schemaProperties $ schema))]

toClassMembers :: HashMap Text Schema -> [Decl]
toClassMembers m =
  [toField, toGetter, toSetter] <*> toList m

toField :: (Text, Schema) -> Decl
toField (name, schema) =
  MemberDecl $
    FieldDecl
      [Private]
      (RefType . ClassRefType . toType $ (name, schema))
      [VarDecl (VarId (Ident . unpack $ name)) Nothing]

toInnerClass :: (Text, Schema) -> Decl
toInnerClass (name, schema) =
  undefined

toGetter :: (Text, Schema) -> Decl
toGetter (name, schema) =
  MemberDecl $
    MethodDecl
      [Public]
      []
      (Just . RefType . ClassRefType . toType $ (name, schema))
      (Ident . (++) "get" . unpack . toTitle $ name)
      []
      []
      (MethodBody . Just $ Block [BlockStmt . Return . Just . FieldAccess $ PrimaryFieldAccess This (Ident . unpack $ name)])

toSetter :: (Text, Schema) -> Decl
toSetter (name, schema) =
  MemberDecl $
    MethodDecl
    [Public]
    []
    Nothing
    (Ident . (++) "set" . unpack . toTitle $ name)
    [FormalParam [] (RefType . ClassRefType . toType $ (name, schema)) False (VarId . Ident . unpack $ name)]
    []
    (MethodBody . Just $ Block
      [BlockStmt . ExpStmt $
        Assign
        (FieldLhs . PrimaryFieldAccess This . Ident . unpack $ name)
        EqualA . ExpName $ Name [Ident . unpack $ name]])


toType :: (Text, Schema) -> ClassType
toType (name, schema) =
  case _schemaType schema of
    Just (TypeValidatorString "string") ->
      simpleClassType "String"

    Just (TypeValidatorString "boolean") ->
      simpleClassType "Boolean"

    Just (TypeValidatorString "number") ->
      simpleClassType "Double"

    Just (TypeValidatorString "object") ->
      simpleClassType . unpack . toTypeIdent $ name

    Just (TypeValidatorString "array") ->
      complexClassType "List" schema

    _ ->
      simpleClassType "Object"
  where
    simpleClassType :: String -> ClassType
    simpleClassType t =
      ClassType [(Ident t, [])]

    complexClassType :: String -> Schema -> ClassType
    complexClassType t s =
      ClassType [
        (Ident t,
        [ActualType . ClassRefType $ toType (name, fromJust . toSubschema $ s)])
      ]

    toSubschema :: Schema -> Maybe Schema
    toSubschema schema =
      case _schemaItems schema of
        Just (ItemsObject subschema) ->
          Just subschema

        _ ->
          Nothing

toTypeIdent :: Text -> Text
toTypeIdent =
  toTitle . singularize

