module Helper (readSchema, readJava) where

import qualified           Data.ByteString.Lazy as B (toStrict, readFile, ByteString, unpack)
import           Data.Json (decodeJson)
import           Data.JsonSchema.Draft4.Schema (Schema)
import           Data.Aeson
import           Language.Java.Parser
import Language.Java.Syntax

readSchema :: String -> IO Schema
readSchema path = do
  Right json <- decodeJson <$> B.readFile path
  let Success schema = fromJSON json
  return schema

readJava :: String -> IO CompilationUnit
readJava path = do
   Right java <- parser compilationUnit <$> readFile path
   return java
