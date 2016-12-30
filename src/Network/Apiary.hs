{-# LANGUAGE OverloadedStrings #-}

module Network.Apiary
    ( parseApib
    ) where

import Network.Wreq (postWith, defaults, header, Response, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.ByteString.Lazy (ByteString)

parseApib :: ByteString -> IO ByteString
parseApib =
    fmap (^. responseBody) . postWith apiaryOpts apiaryUrl
  where
    apiaryUrl = "https://api.apiblueprint.org/parser"
    apiaryAccept = "application/vnd.refract.parse-result+json"
    apiaryContentType = "text/vnd.apiblueprint"
    apiaryOpts = defaults
                 & header "Accept" .~ [apiaryAccept]
                 & header "Content-Type" .~ [apiaryContentType]
