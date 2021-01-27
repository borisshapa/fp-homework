module Block1.Task1
  (
    -- * The @Block1.Task1@ functions
    stringSum
  ) where

import Text.Read (readMaybe)

-- | Safe sum search function.
-- If at least one element of the string cannot be converted
-- to an integer, the function returns Nothing.
stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words