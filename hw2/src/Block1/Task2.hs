{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 
  ( -- * The @Block1.Task2@ types
    Tree(..)
  ) where

import Control.Applicative (liftA2)

-- | Binary tree
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> tree     = fmap f tree
  Branch l r <*> tree = Branch (l <*> tree) (r <*> tree)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap m (Leaf x)     = m x
  foldMap m (Branch l r) = (foldMap m l) <> (foldMap m r)

instance Traversable Tree where
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA (Leaf x)         = fmap Leaf x
  sequenceA (Branch l r)     = liftA2 Branch (sequenceA l) (sequenceA r)