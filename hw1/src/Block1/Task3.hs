{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Block1.Task3
  ( -- * The @Block1.Task3@ types
    Tree(..)

    -- * The @Block1.Task3@ functions
  , find
  , fromList
  , insert
  , isEmpty
  , remove
  , size
  ) where

import Data.List.NonEmpty (NonEmpty (..), cons, head)

-- | Returns a head of non-empty list
getValue :: NonEmpty a -> a
getValue = Data.List.NonEmpty.head

-- | Binary tree.
data Tree a
  -- | Tree leaf. Contains no data.
  = Leaf

  -- | Tree node. Contains a non-empty list
  -- of the same values and has two children.
  | Node
    { nValues :: NonEmpty a  -- ^ list of identical values
    , nLeft :: Tree a        -- ^ left child
    , nRight :: Tree a       -- ^ right child
    }
  deriving (Show)

instance (Eq a) => Eq (Tree a) where
  -- | Trees are equal if the lists at the root node are equal,
    -- and the right and left subtrees are also equal.
  (==) :: Tree a -> Tree a -> Bool
  Leaf == Leaf = True
  (Node values1 left1 right1) == (Node values2 left2 right2) =
    (values1 == values2) && (left1 == left2) && (right1 == right2)
  _ == _ = False

-- | Checking the tree for emptiness.
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Calculating the size of the tree (the number of elements in it).
size :: Tree a -> Int
size Leaf = 0
size Node{..} = (length nValues) + (size nLeft) + (size nRight)

-- | Search for a given item in a tree
-- (the tree is assumed to be a binary search tree).
find :: Ord a => Tree a -> a -> Maybe (Tree a)
find Leaf _ = Nothing
find node@Node{..} toFind
  | toFind == value = Just node
  | toFind < value  = find nLeft toFind
  | otherwise       = find nRight toFind
  where
    value = getValue nValues

-- | Insert a new item into a binary search tree.
-- If the inserted element is already in the tree,
-- then it is added to the list of the node in which this element is located.
insert :: Ord a => Tree a -> a -> Tree a
insert Leaf toInsert = Node (toInsert :| []) Leaf Leaf
insert Node {..} toInsert
  | toInsert == value = Node (cons toInsert nValues) nLeft nRight
  | toInsert < value  = Node nValues (insert nLeft toInsert) nRight
  | otherwise         = Node nValues nLeft (insert nRight toInsert)
  where
    value = getValue nValues

-- | Creates a tree from a list of elements.
fromList :: Ord a => [a] -> Tree a
fromList = foldl insert Leaf

-- | Removes the specified item from the tree.
remove :: Ord a => Tree a -> a -> Tree a
remove Leaf _ = Leaf
remove node@Node{..} toRemove
  | toRemove == value =
    case values of
      -- | deleting the node and rehanging the tree
      [] -> rehanging  node
      -- | just deleting of one element from list
      (x : xs) -> Node (x :| xs) nLeft nRight
  | toRemove < value  = Node nValues (remove nLeft toRemove) nRight
  | toRemove > value  = Node nValues nLeft (remove nRight toRemove)
  where
    (value :| values) = nValues

-- | If a node is removed, the vertex with
-- the smallest value from the right subtree is
-- put in its place.
rehanging :: Ord a => Tree a -> Tree a
rehanging Leaf = Leaf
rehanging Node{ nLeft = Leaf, .. } = nRight
rehanging Node{ nRight = Leaf, .. } = nLeft
rehanging Node{..} =
  Node (leftistValues nRight) nLeft (remove nRight (getValue leftist))
    where
      leftist = leftistValues nRight

-- | Returns the list with the lowest value.
leftistValues :: Ord a => Tree a -> NonEmpty a
leftistValues Leaf = error "cannot get values from leaf"
leftistValues Node{ nLeft = Leaf, .. } = nValues
leftistValues Node{..}                 = leftistValues nLeft