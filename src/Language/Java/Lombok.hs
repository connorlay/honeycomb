module Language.Java.Lombok (generateAst, Schema) where

import Language.Java.Syntax
import Data.Aeson (Value)

type Schema = Value

generateAst :: Schema -> CompilationUnit
generateAst schema =
  CompilationUnit Nothing [] [
    ClassTypeDecl (ClassDecl [Public] (Ident "Complex") [] Nothing [] (
      ClassBody
        [ MemberDecl (FieldDecl
            [Private]
            (RefType (ClassRefType (ClassType [(Ident "String", [])])))
            [VarDecl (VarId (Ident "id")) Nothing])
        ]
     ))
   ]
