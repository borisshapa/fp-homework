module Block2.Task2
  ( -- * The @Block2.Task2@ functions
    moving
  ) where

import Control.Monad.State (State, evalState, get, put)

data MoveState a = MoveState
  { msProcessed :: [a]
  , msUnprocessed :: [a]
  , msSum :: a
  , msCount :: Int
  }

movePrefix :: Fractional a => State (MoveState a) a
movePrefix = do
  curState@MoveState{msUnprocessed = unprocessed} <- get
  let sum = (msSum curState) + head unprocessed
  let count = (msCount curState) + 1
  put (curState { msUnprocessed = tail unprocessed,
    msSum = sum, msCount = count })
  return (sum / realToFrac count)

move :: Fractional a => State (MoveState a) a
move = do
  curState@MoveState{msProcessed=processed, msUnprocessed=unprocessed} <- get
  let sum = (msSum curState) + (head unprocessed) - (head processed)
  put (curState { msProcessed = (tail processed),
    msUnprocessed = (tail unprocessed), msSum = sum})
  return (sum / realToFrac (msCount curState))

-- | A [SMA calculation](https://en.wikipedia.org/wiki/Moving_average)
moving :: Fractional a => Int -> [a] -> [a]
moving batch xs = evalState
    (sequence . take (length xs) $ procPrefix ++ procTail) $ MoveState xs xs 0 0
      where
        procPrefix = replicate batch movePrefix
        procTail   = replicate ((length xs) - batch) move