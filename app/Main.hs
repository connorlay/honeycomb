{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                    (Value)
import           Data.ApiElement
import           Data.ByteString.Lazy     as B (readFile)
import           Data.Json                     (decodeJson)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Language.Java
import           Language.Java.Pretty
import           Network.Apiary                (parseApib)
import           Search.DepthFirstSearch
import           System.Environment
import           System.Exit
import           Data.Maybe                    (catMaybes)
import           Data.Validator.Reference

main :: IO ()
main =
  route =<< getArgs

route :: [String] -> IO ()
route args =
  case args of
    [x] ->
      generate x
    _ ->
      help

generate :: String -> IO ()
generate path = do
  resp <- parseApib =<< B.readFile path
  let Right v = decodeJson resp
  let schemas = parseJsonSchemas . findJsonSchemas $ v
  mapM_ putStrLn $ catMaybes $ map (fmap (prettyPrint . toJava "Simple")) schemas
  exitSuccess

help :: IO ()
help =
  putStrLn "\n\tUsage: honeycomb <path-to-blueprint>\n"

findJsonSchemas :: Value -> [Value]
findJsonSchemas x =
  traverseAst x isASchema

parseJsonSchemas :: [Value] -> [Maybe Schema]
parseJsonSchemas =
  map asJsonSchema
