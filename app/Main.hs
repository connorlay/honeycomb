module Main where

import Network.Apiary (parseApib)
import Data.Apiary (decodeJson)
import Data.ByteString.Lazy as B (readFile)

main :: IO ()
main = do
  resp <- parseApib =<< B.readFile "simple.apib"
  putStrLn . show . decodeJson $ resp
