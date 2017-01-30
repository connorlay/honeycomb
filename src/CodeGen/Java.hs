{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Java (toJava) where

import           Data.HashMap.Lazy             (HashMap (..), empty, toList)
import           Data.JsonSchema.Draft4.Schema (Schema (..))
import           Data.Maybe                    (fromJust, fromMaybe, mapMaybe)
import           Data.Text                     (Text (..), toTitle, unpack)
import           Data.Validator.Draft4.Any     (TypeValidator (..))
import           Data.Validator.Draft4.Array   (Items (..))
import           Language.Java.Syntax
import           Text.Countable                (singularize)

{- TODO: refactor to include error messages -}
toJava :: Text -> Schema -> CompilationUnit
toJava name schema =
  CompilationUnit Nothing
    [ImportDecl False (Name [Ident "java",Ident "util",Ident "List"]) False]
    [ClassTypeDecl . toClass [Public] $ (name, schema)]

toClass :: [Modifier] -> (Text, Schema) -> ClassDecl
toClass modifiers (name, schema) =
  ClassDecl modifiers (Ident . unpack . toTypeIdent $ name) [] Nothing []
    (ClassBody . toClassMembers . fromMaybe empty . _schemaProperties $ schema)

toClassMembers :: HashMap Text Schema -> [Decl]
toClassMembers m =
  fieldsAndMethods m ++ innerClasses m

  where
    innerClasses :: HashMap Text Schema -> [Decl]
    innerClasses =
      mapMaybe toInnerClass . toList

    fieldsAndMethods :: HashMap Text Schema -> [Decl]
    fieldsAndMethods m =
      [toField, toGetter, toSetter] <*> toList m

toField :: (Text, Schema) -> Decl
toField (name, schema) =
  MemberDecl $
    FieldDecl [Private] (RefType . ClassRefType . toType $ (name, schema))
      [VarDecl (VarId (Ident . unpack $ name)) Nothing]

{- TODO: refactor to remove unsafe fromJust call -}
toInnerClass :: (Text, Schema) -> Maybe Decl
toInnerClass (name, schema) =
  case _schemaType schema of
    Just (TypeValidatorString "object") ->
      Just .
        MemberDecl .
          MemberClassDecl .
            toClass [Public, Static] $ (name, schema)

    Just (TypeValidatorString "array") ->
      toInnerClass (name, fromJust . toSubschema $ schema)

    _ ->
      Nothing

toGetter :: (Text, Schema) -> Decl
toGetter (name, schema) =
  MemberDecl $
    MethodDecl
      [Public]
      []
      (Just . RefType . ClassRefType . toType $ (name, schema))
      (Ident . getterName $ name)
      []
      []
      (MethodBody . Just $ Block
         [ BlockStmt .
             Return .
               Just .
                 FieldAccess .
                   PrimaryFieldAccess This
                     $ (Ident . unpack $ name)
         ])

  where
    getterName :: Text -> String
    getterName = (++) "get" . unpack . toTitle

toSetter :: (Text, Schema) -> Decl
toSetter (name, schema) =
  MemberDecl $
    MethodDecl
      [Public]
      []
      Nothing
      (Ident . setterName $ name)
      [ FormalParam
          []
          (RefType . ClassRefType . toType $ (name, schema))
          False
          (VarId . Ident . unpack $ name) ]
      []
      (MethodBody . Just . Block $
         [ BlockStmt . ExpStmt $
           Assign
             (FieldLhs . PrimaryFieldAccess This . Ident . unpack $ name)
             EqualA . ExpName $ Name [Ident . unpack $ name]
         ])

  where
    setterName :: Text -> String
    setterName = (++) "set" . unpack . toTitle

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
