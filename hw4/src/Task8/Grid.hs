{-# LANGUAGE InstanceSigs #-}

module Task8.Grid
  ( -- * The @Task8.Grid@ types
    Grid (..)

    -- * The @Task8.Grid@ functions
  , down
  , gridRead
  , gridWrite
  , horizontal
  , left
  , neighbours
  , right
  , up
  , vertical
  ) where

import Control.Comonad (Comonad (..))

import Task8.ListZipper

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance (Show a) => Show (Grid a) where
  show :: Grid a -> String
  show (Grid lz) = unlines $ map show (toList 10 lz)

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = mkZipper up down

neighbours :: [Grid a -> Grid a]
neighbours = [up, right, down, left]

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical