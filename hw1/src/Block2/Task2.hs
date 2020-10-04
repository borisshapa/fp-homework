{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block2.Task2
  ( -- * The @Block2.Task2@ functions
    joinSplitId
  , joinWith
  ,  splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), cons, head)

-- | Splits the list into sublists by item.
-- Always returns a non-empty list of elements.
splitOn :: forall a. Eq a => a -> [a] -> NonEmpty [a]
splitOn separator toSplit = foldr split ([] :| []) toSplit
  where
    split :: a -> NonEmpty [a] -> NonEmpty [a]
    split element list@(x :| xs)
      | element == separator = cons [] list
      | otherwise            = (element : x) :| xs

-- | Concatenates the list by separating its elements
-- with the special delimiter passed.
joinWith :: forall a. a -> NonEmpty [a] -> [a]
joinWith separator list = foldr1 join list
  where
    join :: [a] -> [a] -> [a]
    join element joinList = element ++ separator : joinList

-- | Concatination of join and split is identity.
joinSplitId :: Eq a => NonEmpty a -> [a]
joinSplitId list@(x :| xs) = (joinWith separator . splitOn separator) (x : xs)
  where
    separator = Data.List.NonEmpty.head list