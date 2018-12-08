module D8 (
    buildTree
  , Node(..)
) where

import Data.List
import Data.Ord
import Data.Function

data Node =
    Node {
        childCnt :: Int
      , dataCnt  :: Int
      , nChild   :: [Node]
      , nData    :: [Int]
    } deriving(Show, Eq)

buildTree :: [Int] -> [Node]
buildTree = fst . addNode 1

addNode :: Int -> [Int] -> ([Node], [Int])
addNode 0 l = ([], l)
addNode cnt (c:d:xs) = addN cnt c d (addNode c xs)
    where
        addN cnt c d (children, xs) = nodes (Node c d children (take d xs)) (addNode (cnt - 1) $ drop d xs)
        nodes n (siblings, xs) = (n:siblings, xs)