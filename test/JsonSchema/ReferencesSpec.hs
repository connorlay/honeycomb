{-# LANGUAGE OverloadedStrings #-}

module JsonSchema.ReferencesSpec (spec) where

import           Test.Hspec
import           Helper
import           JsonSchema.References

spec :: Spec
spec =
  describe "Resolving json schema references" $ do
    let path = "./test/Resources/JsonSchema/example_with_refs.json"
    let refs = collectRefs <$> readSchema path

    it "should ignore duplicate refs" $ do
      let size = length <$> refs
      size `shouldReturn` 2

    it "should find all reference strings" $ do
      refs `shouldReturn` ["#/definitions/cat", "#/definitions/person"]

