{-# LANGUAGE OverloadedStrings #-}

module CodeGen.JavaSpec (spec) where

import           CodeGen.Java        (toJava)
import           Language.Java.Pretty (prettyPrint)
import           Test.Hspec
import           Helper
import           Debug.Trace

spec :: Spec
spec =
  describe "Generating Java code from a schema" $ do
    let path = "./test/Resources/JsonSchema/example.json"
    let java = toJava "Example" <$> readSchema path

    it "should generate a Java class" $ do
      expected <- readJava "./test/Resources/Java/Example.java"
      java `shouldReturn` expected
