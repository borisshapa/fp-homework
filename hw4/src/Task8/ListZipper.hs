{-# LANGUAGE InstanceSigs #-}

module Task8.ListZipper
  ( -- * The @Task8.ListZipper@ types
    ListZipper (..)

    -- * The @Task8.ListZipper@ functions
  , iterateTail
  , listLeft
  , listRight
  , listWrite
  , mkZipper
  , toList
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance (Show a) => Show (ListZipper a) where
  show :: ListZipper a -> String
  show lz = unwords $ map show (toList 10 lz)

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a : as) x bs)  = LZ as a (x : bs)
listLeft _ = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper genLeft genRight e =
  LZ (iterateTail genLeft e) e (iterateTail genRight e)

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = mkZipper listLeft listRight