{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Lombok (generateAst) where

import           Data.Aeson                    (Value)
import           Data.HashMap.Lazy             (HashMap, toList)
import           Data.JsonSchema.Draft4.Schema (Schema (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text, toTitle, unpack)
import           Data.Validator.Draft4.Any     (TypeValidator (..))
import           Data.Validator.Draft4.Array   (Items (..))
import           Language.Java.Syntax
import           Text.Countable                (singularize)

{-
generateAst :: Schema -> CompilationUnit
generateAst schema =
  CompilationUnit
    Nothing
    [ ImportDecl False (Name [Ident "java", Ident "util", Ident "List"]) False
    , ImportDecl False (Name [Ident "lombok", Ident "Data"]) False
    ]
    [ ClassTypeDecl
        (ClassDecl
           [Annotation MarkerAnnotation { annName = Name [Ident "Data"] }, Public]
           (Ident "Complex")
           []
           Nothing
           []
           (ClassBody
              [ MemberDecl
                  (FieldDecl [Private] (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                     [VarDecl (VarId (Ident "id")) Nothing])
              , MemberDecl
                  (FieldDecl
                     [Private]
                     (RefType
                        (ClassRefType
                           (ClassType
                              [ (Ident "List", [ ActualType
                                                   (ClassRefType (ClassType [(Ident "Widget", [])]))
                                               ])
                              ])))
                     [VarDecl (VarId (Ident "widgets")) Nothing])
              , MemberDecl
                  (MemberClassDecl
                     (ClassDecl
                        [ Annotation MarkerAnnotation { annName = Name [Ident "Data"] }
                        , Public
                        , Static
                        ]
                        (Ident "Widget")
                        []
                        Nothing
                        []
                        (ClassBody
                           [ MemberDecl
                               (FieldDecl
                                  [Public]
                                  (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                                  [VarDecl (VarId (Ident "id")) Nothing])
                           ])))
              ]))
    ]
-}
generateAst :: Schema -> CompilationUnit
generateAst schema =
  CompilationUnit
    Nothing
    [ ImportDecl False (Name [Ident "java", Ident "util", Ident "List"]) False
    , ImportDecl False (Name [Ident "lombok", Ident "Data"]) False
    ]
    [ ClassTypeDecl
        (ClassDecl
           [Annotation MarkerAnnotation { annName = Name [Ident "Data"] }, Public]
           (Ident "Complex")
           []
           Nothing
           []
           (ClassBody . toFields $ schema))
    ]

toFields :: Schema -> [Decl]
toFields schema =
  case _schemaProperties schema of
    Nothing -> []
    Just m  -> map toField . toList $ m

type JavaType = (Ident, [TypeArgument])

type PropName = Text

toField :: (PropName, Schema) -> Decl
toField (propName, schema) =
  MemberDecl
    (FieldDecl [Private] (RefType (ClassRefType (ClassType [toJavaType (propName, schema)])))
       [VarDecl (VarId (Ident . unpack $ propName)) Nothing])

toJavaType :: (PropName, Schema) -> JavaType
toJavaType (propName, schema) =
  case _schemaType schema of
    Just (TypeValidatorString "string") -> (Ident "String", [])
    Just (TypeValidatorString "boolean") -> (Ident "Boolean", [])
    Just (TypeValidatorString "number") -> (Ident "Double", [])
    Just (TypeValidatorString "object") -> (Ident . toJavaClassName $ propName, [])
    Just (TypeValidatorString "array") -> (Ident "List", [ ActualType
                                                             (ClassRefType
                                                                (ClassType
                                                                   [ toJavaType
                                                                       (propName, fromJust . toArraySubtype $ schema)
                                                                   ]))
                                                         ])
    _ -> (Ident "Object", [])

toArraySubtype :: Schema -> Maybe Schema
toArraySubtype schema =
  case _schemaItems schema of
    Just (ItemsObject subschema) -> Just subschema
    _                            -> Nothing

toJavaClassName :: Text -> String
toJavaClassName =
  unpack . toTitle . singularize
