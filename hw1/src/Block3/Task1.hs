{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  ( -- * The @Block3.Task1@ functions
    eitherConcat
  , maybeConcat
  ) where

import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)

-- | Takes a list of lists inside Maybe and
-- returns the concatenation of all the inner lists.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = concat (catMaybes list)

-- | The function takes an arbitrary set of Either,
-- where both Left and Right contain some monoidal elements,
-- and returns a pair of the results of a monoidal concatenation
-- of separately elements inside Left and separately elements inside Right.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat eitherList = (left, right)
  where
    left = mconcat (lefts eitherList)
    right = mconcat (rights eitherList)