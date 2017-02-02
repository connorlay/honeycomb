{-# LANGUAGE OverloadedStrings #-}

module JsonSchema.ReferencesSpec (spec) where

import           Test.Hspec
import           Helper
import           JsonSchema.References
import           Data.HashMap.Lazy (fromList, HashMap(..))
import           Data.JsonSchema.Draft4.Schema (Schema (..), emptySchema)
import           Data.Text (Text)

spec :: Spec
spec =
  describe "JsonSchema.References" $ do

    describe "Collecting json schema refs" $ do
      let path = "./test/Resources/JsonSchema/example_with_refs.json"
      let refs = collectRefs <$> readSchema path

      it "should ignore duplicate refs" $ do
        let size = length <$> snd <$> refs
        size `shouldReturn` 2

      it "should find all ref strings" $ do
        (snd <$> refs) `shouldReturn` ["#/definitions/cat", "#/definitions/person"]

    describe "Resolving json schema refs" $ do
      let path = "./test/Resources/JsonSchema/example_with_refs.json"
      let refMap = resolveRefs <$> collectRefs <$> readSchema path

      it "should resolve all referenced schemas" $ do
        let expected = fromList [ ("#/definitions/person", emptySchema { _schemaId = Just "http://example.com/person" })
                                , ("#/definitions/cat", emptySchema { _schemaId = Just "http://example.com/cat" })
                                ]
        (snd <$> refMap) `shouldReturn` expected
