{-# LANGUAGE OverloadedStrings #-}

module Language.Java (toJava) where

import Data.HashMap.Lazy (HashMap(..), empty, toList)
import Data.Text (Text(..), toTitle, unpack)
import Language.Java.Syntax
import Data.JsonSchema.Draft4.Schema (Schema (..))
import Text.Countable (singularize)
import Data.Maybe (fromMaybe)

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
  undefined

toSetter :: (Text, Schema) -> Decl
toSetter (name, schema) =
  undefined

toType :: (Text, Schema) -> ClassType
toType schema =
  undefined

toTypeIdent :: Text -> Text
toTypeIdent =
  toTitle . singularize

