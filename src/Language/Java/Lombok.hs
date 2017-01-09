{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Lombok (generateAst) where

import           Data.Aeson                    (Value)
import           Data.HashMap.Lazy             (HashMap, toList)
import           Data.JsonSchema.Draft4.Schema (Schema (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, unpack)
import           Data.Validator.Draft4.Any     (TypeValidator (..))
import           Language.Java.Syntax

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
           (ClassBody . toFields $ schema))]

toFields :: Schema -> [Decl]
toFields schema =
  case _schemaProperties schema of
    Nothing -> []
    Just m  -> map toField . toList $ m

type Property = (Text, Schema)

toField :: Property -> Decl
toField (name, schema) =
  let
    javaType = fromMaybe "Object" (jsToJava <$> _schemaType schema)
    in MemberDecl
        (FieldDecl
          [Private]
          (RefType (ClassRefType (ClassType [(Ident . unpack $ javaType, [])])))
          [VarDecl (VarId (Ident . unpack $ name)) Nothing])

jsToJava :: TypeValidator -> Text
jsToJava js =
  case js of
    TypeValidatorString "string"  -> "String"
    TypeValidatorString "array"   -> "List"
    TypeValidatorString "number"  -> "Double"
    TypeValidatorString "boolean" -> "Boolean"
    _                             -> "Object"
