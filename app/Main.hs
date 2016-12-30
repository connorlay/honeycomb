module Main where

import           Data.ByteString.Lazy as B (readFile)
import           Data.Json            (decodeJson)
import           Network.Apiary       (parseApib)

main :: IO ()
main = do
  resp <- parseApib =<< B.readFile "simple.apib"
  print . decodeJson $ resp
