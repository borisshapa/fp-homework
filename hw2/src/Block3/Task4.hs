module Block3.Task4
  ( -- * The @Block3.Task4@ functions
    listlistParser
  ) where

import Control.Applicative (many, (<|>))
import Data.Char (isSpace)

import Block3.Task1 (Parser(..))
import Block3.Task2 (element, eof, satisfy)
import Block3.Task3 (integerParser)

-- | Skipping whitespace characters
skipWhitespaces :: Parser Char ()
skipWhitespaces = const () <$> (many (satisfy isSpace))

-- | Parsing the integer and skipping whitespace characters before and after it
parseIntegerAndSkipWs :: Parser Char Int
parseIntegerAndSkipWs = do
  skipWhitespaces
  res <- integerParser
  skipWhitespaces
  return res

-- | Skipping ',' symbol
comma :: Parser Char ()
comma = const () <$> (element ',')

-- | Parsing n numbers separated by commas
parseNIntegers :: Int -> Parser Char [Int]
parseNIntegers 0 = return []
parseNIntegers n =
  (:) <$ comma <*> parseIntegerAndSkipWs <*> parseNIntegers (n - 1)

-- | Parsing integer list
listParser :: Parser Char [Int]
listParser = do
  size <- parseIntegerAndSkipWs
  list <- parseNIntegers size
  return list

-- | Parsing integer list list
listlistParser :: Parser Char [[Int]]
listlistParser = do
    firstList <- listParser
    lists <- many (comma *> listParser)
    return (firstList : lists)
  <|> do
    skipWhitespaces
    return []
  <* eof