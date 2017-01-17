{-# LANGUAGE OverloadedStrings #-}

module Language.JavaSpec (spec) where

import           Data.Aeson
import           Data.HashMap.Strict  (fromList)
import           Language.Java        (toJava)
import           Language.Java.Pretty (prettyPrint)
import           Test.Hspec

spec :: Spec
spec =
  describe "Generating Java code from a schema" $ do
    let Success schema = fromJSON $ Object
                                      (fromList
                                         [ ("title", String "Complex")
                                         , ("type", String "object")
                                         , ("properties", Object
                                                            (fromList
                                                               [ ("widgets", Object
                                                                               (fromList
                                                                                  [ ("items", Object
                                                                                                (fromList
                                                                                                   [ ("title", String
                                                                                                                 "Widget")
                                                                                                   , ("type", String
                                                                                                                "object")
                                                                                                   , ("properties", Object
                                                                                                                      (fromList
                                                                                                                         [ ("id", Object
                                                                                                                                    (fromList
                                                                                                                                       [ ("type", String
                                                                                                                                                    "string")
                                                                                                                                       ]))
                                                                                                                         ]))
                                                                                                   ]))
                                                                                  , ("type", String
                                                                                               "array")
                                                                                  ]))
                                                               , ("id", Object
                                                                          (fromList
                                                                             [ ("type", String
                                                                                          "string")
                                                                             ]))
                                                               ]))
                                         ])

    it "should generate a Java class" $
      prettyPrint (toJava "Complex" schema) `shouldBe` "import java.util.List;\nimport lombok.Data;\n@Data\n public class Complex\n{\n  private String id;\n  private List<Widget> widgets;\n  @Data\n  public static class Widget\n  {\n    public String id;\n  }\n}"
