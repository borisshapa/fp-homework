module Block2.Task1Spec
  (
    spec
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Sum(..))

import Block1.Task3 (Tree(..), fromList)
import Block2.Task1

import Test.Hspec

spec :: Spec
spec = do
  let singleNode = Node (2 :| [2, 2]) Leaf Leaf
  let tree =
        Node
          (5 :| [])
          (Node
            (3 :| [])
            (Node (1 :| []) Leaf Leaf)
            (Node (4 :| []) Leaf Leaf)
          )
          (Node (7 :| []) (Node (6 :| []) Leaf Leaf) Leaf)
  let emptyTree = Leaf

  describe "sort" $ do
    it "sort a list" $ do
      toList (fromList [5, 3, 7, 1, 4, 6]) `shouldBe` [1, 3, 4, 5, 6, 7]

    it "sort an empty list" $ do
      toList (fromList ([] :: [Int])) `shouldBe` []

  describe "foldMap" $ do
    it "creating Sum from empty tree" $ do
      foldMap Sum emptyTree `shouldBe` Sum 0

    it "creating Sum from tree with one node" $ do
      foldMap Sum singleNode `shouldBe` Sum 6

    it "creating Sum from tree with many nodes" $ do
      foldMap Sum tree `shouldBe` Sum 26

  describe "foldr" $ do
    it "sum of empty tree values" $ do
      foldr (+) 0 emptyTree `shouldBe` 0

    it "sum of empty tree values" $ do
      foldr (+) 0 singleNode `shouldBe` 6

    it "sum of tree values" $ do
      foldr (+) 0 tree `shouldBe` 26

    it "product of tree values" $ do
      foldr (*) 1 tree `shouldBe` 2520

  describe "foldl" $ do
    it "sum of empty tree values" $ do
      foldl (+) 0 emptyTree `shouldBe` 0

    it "sum of empty tree values" $ do
      foldl (+) 0 singleNode `shouldBe` 6

    it "sum of tree values" $ do
      foldl (+) 0 tree `shouldBe` 26

    it "product of tree values" $ do
      foldl (*) 1 tree `shouldBe` 2520