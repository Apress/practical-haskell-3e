{-@ LIQUID "--no-termination" @-}
module MyLib (someFunc) where

import Language.Haskell.Liquid.Prelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

{-@ measure treeSize @-}
{-@ treeSize :: BinaryTree a -> Nat @-}
treeSize :: BinaryTree a -> Int
treeSize Leaf         = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

{-@ treeInsert :: x: a -> v: BinaryTree a
               -> {w: BinaryTree a | treeSize w = treeSize v + 1} @-}
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | x <= y    = Node y l r
                          | otherwise = Node y l (treeInsert y r)