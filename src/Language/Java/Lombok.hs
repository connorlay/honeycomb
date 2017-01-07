module Language.Java.Lombok (generateAst) where

import           Data.Aeson                    (Value)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Language.Java.Syntax

generateAst :: Schema -> CompilationUnit
generateAst schema =
  CompilationUnit
    Nothing
    [ ImportDecl False (Name [(Ident "java"), (Ident "util"), (Ident "List")]) False
    , ImportDecl False (Name [(Ident "lombok"), (Ident "Data")]) False
    ]
    [ ClassTypeDecl
        (ClassDecl
           [Annotation (MarkerAnnotation { annName = Name [Ident "Data"] }), Public]
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
                        [ Annotation (MarkerAnnotation { annName = Name [Ident "Data"] })
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
