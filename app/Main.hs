module Main where

import           Data.Aeson                    (Value)
import           Data.ApiElement
import           Data.ByteString.Lazy          as B (readFile)
import           Data.Json                     (decodeJson)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Network.Apiary                (parseApib)
import           Search.DepthFirstSearch

main :: IO ()
main = do
  resp <- parseApib =<< B.readFile "simple.apib"
  case decodeJson resp of
    Left msg ->
      print msg
    Right v ->
      print . parseJsonSchemas . findJsonSchemas $ v

findJsonSchemas :: Value -> [Value]
findJsonSchemas x =
  traverseAst x isASchema

parseJsonSchemas :: [Value] -> [Maybe Schema]
parseJsonSchemas xs =
  map (asJsonSchema) xs
