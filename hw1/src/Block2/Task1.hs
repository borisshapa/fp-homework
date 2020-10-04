{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Block2.Task1 where

import Block1.Task3 (Tree(..))

instance Foldable Tree where
  -- | Assembles a tree into a monoid.
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f Node{..} =
    (foldMap f nLeft) `mappend` foldMap f nValues `mappend` foldMap f nRight

  -- | Right fold in the natural sense.
  -- Always folds the right subtree first.
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z Node{..} =
    foldr f (foldr f (foldr f z nRight) nValues) nLeft