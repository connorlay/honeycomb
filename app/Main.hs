module Main where

import           Data.Apiary          (decodeJson)
import           Data.ByteString.Lazy as B (readFile)
import           Network.Apiary       (parseApib)

main :: IO ()
main = do
  resp <- parseApib =<< B.readFile "simple.apib"
  putStrLn . show . decodeJson $ resp
