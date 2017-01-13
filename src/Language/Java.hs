{-# LANGUAGE OverloadedStrings #-}

module Language.Java (toJava) where

import Data.HashMap.Lazy (HashMap(..), empty)
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
  undefined

toField :: Text -> Schema -> Decl
toField name schema =
  undefined

toInnerClass :: Text -> Schema -> Decl
toInnerClass name schema =
  undefined

typeOf :: Schema -> (Ident, [TypeArgument])
typeOf schema =
  undefined

toClassIdent :: Text -> Text
toClassIdent =
  toTitle . singularize

