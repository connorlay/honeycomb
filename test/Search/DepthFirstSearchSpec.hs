{-# LANGUAGE OverloadedStrings #-}

module Search.DepthFirstSearchSpec (spec) where

import           Test.Hspec

import           Data.Aeson
import           Data.HashMap.Strict     as HMS (fromList)
import           Data.Vector             as V (fromList)
import           Search.DepthFirstSearch

spec :: Spec
spec =
  describe "Traversing an Ast" $ do
    let ast = Object
                (HMS.fromList
                   [ ("a", String "apple")
                   , ("b", String "banana")
                   , ("c", Array
                             (V.fromList
                                [ String "cherry"
                                , Object
                                    (HMS.fromList [("d", String "durian"), ("e", String "evil")])
                                ]))
                   ])
    let found = traverseAst ast isLeaf

    it "should visit each node once" $
      length found `shouldBe` 5

    it "should find all leaves" $
      found `shouldBe` [ String "evil"
                       , String "durian"
                       , String "cherry"
                       , String "banana"
                       , String "apple"
                       ]
