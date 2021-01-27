{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  ( -- * The @Block3.Task1@ types
    Parser(..)
  ) where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Bifunctor (first)

-- | Parser data type
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parse) = Parser ((fmap $ first f) . parse)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser (pure . (,) a)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  parser1 <*> parser2 = do
    res1 <- parser1
    res2 <- parser2
    return (res1 res2)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser parse >>= f =
    Parser (\input -> (parse input) >>= uncurry (runParser . f))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (Parser parse1) <|> (Parser parser2) =
    Parser (\input -> parse1 input <|> parser2 input)