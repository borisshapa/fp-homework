{-# LANGUAGE LambdaCase #-}

module Block3.Task2
  ( -- * The @Block3.Task2@ functions
    element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Block3.Task1 (Parser(..))

-- | Parser never crashes and does not consume the input
ok :: Parser s ()
ok = return ()

-- | Parser checks that the parser has reached the end of the data stream
-- (otherwise it crashes)
eof :: Parser s ()
eof = Parser (\case
  [] -> Just((), [])
  _ -> Nothing)

-- | Parser takes a predicate for an element of the stream,
-- and returns the element, absorbing it from the stream,
-- if the predicate for an element is True, otherwise it falls
satisfy :: (s -> Bool) -> Parser s s
satisfy predicate = Parser (\case
  (x:xs) -> if predicate x then Just (x, xs) else Nothing
  [] -> Nothing)

-- | Parses one stream element
element :: Eq s => s -> Parser s s
element = satisfy . (==)

-- | Parses multiple stream elements
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element