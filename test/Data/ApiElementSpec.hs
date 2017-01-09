{-# LANGUAGE OverloadedStrings #-}

module Data.ApiElementSpec (spec) where

import           Data.Aeson
import           Data.ApiElement
import           Data.HashMap.Strict as HMS (fromList)
import           Data.Vector         as V (fromList)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Detecting json schema objects" $ do
    context "when there is a schema" $ do
      let ast = Object
                  (HMS.fromList
                     [ ("content", String
                                     "{\n    \"type\": \"object\",\n    \"properties\": {\n        \"id\": {\n            \"type\": \"string\"\n        },\n        \"title\": {\n            \"type\": \"string\"\n        },\n        \"content\": {\n            \"type\": \"string\"\n        },\n        \"tags\": {\n            \"type\": \"array\",\n            \"items\": {\n                \"type\": \"string\"\n            }\n        }\n    }\n}\n")
                     , ("attributes", Object
                                        (HMS.fromList
                                           [("contentType", String "application/schema+json")]))
                     , ("meta", Object
                                  (HMS.fromList
                                     [("classes", Array (V.fromList [String "messageBodySchema"]))]))
                     , ("element", String "asset")
                     ])

      let Success schema = fromJSON $ Object
                                        (HMS.fromList
                                           [ ("type", String "object")
                                           , ("properties", Object
                                                              (HMS.fromList
                                                                 [ ("id", Object
                                                                            (HMS.fromList
                                                                               [ ("type", String
                                                                                            "string")
                                                                               ]))
                                                                 , ("title", Object
                                                                               (HMS.fromList
                                                                                  [ ("type", String
                                                                                               "string")
                                                                                  ]))
                                                                 , ("content", Object
                                                                                 (HMS.fromList
                                                                                    [ ("type", String
                                                                                                 "string")
                                                                                    ]))
                                                                 , ("tags", Object
                                                                              (HMS.fromList
                                                                                 [ ("type", String
                                                                                              "array")
                                                                                 , ("items", Object
                                                                                               (HMS.fromList
                                                                                                  [ ("type", String
                                                                                                               "string")
                                                                                                  ]))
                                                                                 ]))
                                                                 ]))
                                           ])

      it "should return true" $ do
        isASchema ast `shouldBe` True

      it "should extract the json schema" $ do
        asJsonSchema ast `shouldBe` Just schema

    context "when there is no schema" $ do
      let ast = Object
                  (HMS.fromList
                     [ ("content", String
                                     "{\n    \"id\": \"abc123\",\n    \"title\": \"This is a note\",\n    \"content\": \"This is the note content.\"\n    \"tags\": [\n        \"todo\",\n        \"home\"\n    ]\n}\n")
                     , ("attributes", Object
                                        (HMS.fromList [("contentType", String "application/json")]))
                     , ("meta", Object
                                  (HMS.fromList
                                     [("classes", Array (V.fromList [String "messageBody"]))]))
                     , ("element", String "asset")
                     ])

      it "should return false" $ do
        isASchema ast `shouldBe` False

      it "should not extract the json schema" $ do
        asJsonSchema ast `shouldBe` Nothing
