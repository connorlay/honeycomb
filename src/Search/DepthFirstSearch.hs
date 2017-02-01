module Search.DepthFirstSearch (traverseAst, Expandable(..)) where

import           Data.Aeson
import           Data.HashMap.Strict as HMS (elems)
import           Data.Vector         as V (toList)

class Expandable a where
  expand :: a -> [a]
  isLeaf :: a -> Bool
  isLeaf = null . expand

instance Expandable Value where
  expand v =
    case v of
      Object m -> HMS.elems m
      Array a  -> V.toList a
      _        -> []

{- TODO: swap arguments -}
traverseAst :: (Expandable a) => a -> (a -> Bool) -> [a]
traverseAst ast predicate =
  dfs ast [] []
  where
    dfs node stack acc =
      let stack' = (++) stack . expand $ node
          acc' = if predicate node
                   then node : acc
                   else acc
      in case stack' of
        []   -> acc'
        x:xs -> dfs x xs acc'
