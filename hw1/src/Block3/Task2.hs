{-# LANGUAGE InstanceSigs #-}

module Block3.Task2
  ( -- * The @Block3.Task1@ types
    Endo(..)
  , Name(..)
  , NonEmpty(..)
  , ThisOrThat(..)
  ) where

-- | A list that contains one or more items.
data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
  -- | Сoncatenation of two non-empty lists
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance Eq a => Eq (NonEmpty a) where
  -- | Two non-empty lists are equal if all its elements are equal.
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (x :| xs) == (y :| ys) = (x == y) && (xs == ys)

-- | One of two elements or both.
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show)

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  -- | Equal if the types (this, that, both) are equal,
  -- as well as the values.
  (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
  This a == This b = a == b
  That a == That b = a == b
  Both a1 a2 == Both b1 b2 = (a1 == b1) && (a2 == b2)
  _ == _ = False

instance Semigroup (ThisOrThat a b) where
  -- | Returns an ThisOrThat with a 'right' value equal to
  -- the last 'right' value in a chain and a 'left' value
  -- equal to the last 'left' value in a chain.
  -- If there was no 'right' or 'left' value, then This or That
  -- is returned accordingly.
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This a <> That b = Both a b
  That b <> This a = Both a b
  Both _ b1 <> This a2 = Both a2 b1
  Both a1 _ <> That b2 = Both a1 b2
  _ <> second = second

-- | Stores a line in itself.
data Name = Name String
  deriving (Show)

instance Eq Name where
  -- | Equal if the strings within the names are equal.
  (==) :: Name -> Name -> Bool
  Name a == Name b = a == b

instance Semigroup Name where
  -- | Concatenates if both are not empty,
  -- otherwise returns a non-empty argument or
  -- an empty string if both are empty.
  (<>) :: Name -> Name -> Name
  Name a <> Name "" = Name a
  Name "" <> Name b = Name b
  Name a <> Name b = Name (a ++ "." ++ b)

instance Monoid Name where
  -- | Mempty is Name with empty string.
  mempty :: Name
  mempty = Name ""

-- | A newtype with function that keeps a type.
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  -- | Semigroup operation for Endo is composition of functions.
  (<>) :: Endo a -> Endo a -> Endo a
  a <> b = Endo (getEndo a . getEndo b)

instance Monoid (Endo a) where
  -- | Mempty for Endo is Endo with identity function.
  mempty :: Endo a
  mempty = Endo id