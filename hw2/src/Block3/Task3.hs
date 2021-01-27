module Block3.Task3
  ( -- * The @Block3.Task3@ functions
    integerParser
  , parenthesesParser
  ) where

import Control.Applicative (some, (<|>))
import Data.Char (isDigit)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, ok, satisfy)

-- | Parsing the correct parenthesis sequence
parenthesesParser :: Parser Char ()
parenthesesParser 
  = (element '(' *> parenthesesParser *> element ')' *> parenthesesParser)
  <|> ok

-- | Parsing the integers
integerParser :: Parser Char Int
integerParser = (element '+' *> integer)
      <|> (negate <$> (element '-' *> integer))
      <|> integer
      where
        integer = read <$> some (satisfy isDigit)