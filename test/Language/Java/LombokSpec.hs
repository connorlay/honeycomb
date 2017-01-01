{-# LANGUAGE OverloadedStrings #-}

module Language.Java.LombokSpec (spec) where

import Language.Java.Lombok
import Data.Aeson
import           Data.HashMap.Strict as HMS (fromList)
import           Data.Vector         as V (fromList)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Generating Java code from a schema" $ do
    context "with Lombok annotations" $ do
      undefined
