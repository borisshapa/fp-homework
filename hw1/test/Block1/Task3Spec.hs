module Block1.Task3Spec
  (
    spec
  ) where

import Data.List.NonEmpty (NonEmpty (..))

import Block1.Task3

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

  describe "fromList" $ do
    it "tree from a list with different values" $ do
      fromList [5, 3, 7, 1, 4, 6] `shouldBe` tree

    it "tree from empty list" $ do
      fromList ([] :: [Int]) `shouldBe` Leaf

    it "tree from the list with the same values" $ do
      fromList [2, 2, 2] `shouldBe` singleNode

  describe "isEmpty" $ do
    it "checking for emptiness of tree consisting of one node" $ do
      isEmpty singleNode `shouldBe` False

    it "checking for emptiness of tree consisting of mane nodes" $ do
      isEmpty tree `shouldBe` False

    it "checking for emptiness of empty tree" $ do
      isEmpty emptyTree `shouldBe` True

  describe "size" $ do
    it "size of tree consisting of one node with 3 elements" $ do
      size singleNode `shouldBe` 3

    it "size of tree consisting of many nodes" $ do
      size tree `shouldBe` 6

    it "size of empty tree" $ do
      size emptyTree `shouldBe` 0

  describe "find" $ do
    it "finding an element in tree containing one node" $ do
      find singleNode 2 `shouldBe` Just singleNode

    it "finding an element that is not in tree containing one node" $ do
      find singleNode 3 `shouldBe` Nothing

    it "finding an element in tree containing many nodes" $ do
      find tree 7 `shouldBe` Just
        (Node
          (7 :| [])
          (Node (6 :| []) Leaf Leaf)
          Leaf
        )

    it "finding an element that is not in tree containing many nodes" $ do
      find tree (-128) `shouldBe` Nothing

    it "finding an element in empty tree" $ do
      find emptyTree 0 `shouldBe` Nothing

  describe "insert" $ do
    it "inserting an element that is already in the tree" $ do
      insert singleNode 2 `shouldBe`
        Node
          (2 :| [2, 2, 2])
          Leaf
          Leaf

    it "inserting 3 into tree containing one node" $ do
      insert singleNode 3 `shouldBe`
        Node
          (2 :| [2, 2])
          Leaf
          (Node (3 :| []) Leaf Leaf)

    it "inserting -3 into tree containing one node" $ do
      insert singleNode (-3) `shouldBe`
        Node
          (2 :| [2, 2])
          (Node (-3 :| []) Leaf Leaf)
          Leaf

    it "inserting into tree containing many nodes" $ do
      insert tree 128 `shouldBe`
        Node
          (5 :| [])
          (Node
            (3 :| [])
            (Node (1 :| []) Leaf Leaf)
            (Node (4 :| []) Leaf Leaf)
          )
          (Node
            (7 :| [])
            (Node (6 :| []) Leaf Leaf)
            (Node (128 :| []) Leaf Leaf)
          )

    it "inserting in empty tree" $ do
      insert emptyTree 12 `shouldBe` Node (12 :| []) Leaf Leaf

  describe "remove" $ do
    it "removing from tree with one node" $ do
      remove singleNode 2 `shouldBe`
        Node
          (2 :| [2])
          Leaf
          Leaf

    it "removing from empty tree" $ do
      remove emptyTree 2 `shouldBe` emptyTree

    it "removing from tree with many nodes" $ do
      remove tree 3 `shouldBe`
        Node
          (5 :| [])
          (Node (4 :| []) (Node (1 :| []) Leaf Leaf) Leaf)
          (Node (7 :| []) (Node (6 :| []) Leaf Leaf) Leaf)

    let treeWithoutThree = remove tree 3
    it "removing root from tree with many nodes" $ do
      remove treeWithoutThree 5 `shouldBe`
        Node
          (6 :| [])
          (Node (4 :| []) (Node (1 :| []) Leaf Leaf) Leaf)
          (Node (7 :| []) Leaf Leaf)