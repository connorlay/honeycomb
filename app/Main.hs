{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                    (Value)
import           Data.ApiElement
import           Data.ByteString.Lazy          as B (readFile)
import           Data.Json                     (decodeJson)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Language.Java
import           Language.Java.Pretty
import           Network.Apiary                (parseApib)
import           Search.DepthFirstSearch

main :: IO ()
main = do
  resp <- parseApib =<< B.readFile "simple.apib"
  case decodeJson resp of
    Left msg ->
      print msg
    Right v ->
      print $ map (fmap (prettyPrint . toJava "Simple")) (parseJsonSchemas . findJsonSchemas $ v)

findJsonSchemas :: Value -> [Value]
findJsonSchemas x =
  traverseAst x isASchema

parseJsonSchemas :: [Value] -> [Maybe Schema]
parseJsonSchemas =
  map asJsonSchema
