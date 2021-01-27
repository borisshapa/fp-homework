{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 
  ( -- * The @Block1.Task3@ types
    NonEmpty(..)
  ) where

import Control.Applicative (liftA2)

-- | Non-empty list
data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = (f x) :| (fmap f xs)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = f x :| (fmap f xs ++ fsApplication)
    where
      fsApplication = fs <*> (x : xs)

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x :| xs) >>= f = foldl (<>) (f x) (map f xs)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = (f x) <> (foldMap f xs)

instance Traversable NonEmpty where
  sequenceA :: Applicative f => NonEmpty (f a) -> f (NonEmpty a)
  sequenceA (x :| xs) = liftA2 (:|) x (sequenceA xs)