{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Task2.MonteCarlo
  ( -- * The @Task2.MonteCarlo@ functions
    integrate
  , parallelIntegrate
  , randomX
  ) where

import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (InclusiveRange(..), parMapReduceRange)
import System.Random (mkStdGen, randomRs)

-- | Function to integrate: f (x) = (1 / tg (x ^ 2)) - (cos x)
fun :: Double -> Double
fun x = 1 / (tan $ x ^ (2 :: Int)) - (cos x)

-- | Takes the interval of integration, the number of points,
-- and the sum of the values of the function at these points.
-- Returns the integral calculated by the Monte Carlo method.
monteCarloIntegral :: Double -> Double -> Int -> Double -> Double
monteCarloIntegral a b n sumN = ((b - a) / (fromIntegral n)) * sumN

-- | Takes the interval of integration and the number of points.
-- Returns the integral calculated by the Monte Carlo method.
integrate :: Double -> Double -> Int -> Double
integrate a b accuracy =
  let xs = randomX a b accuracy 239
      ys = map fun xs
  in  monteCarloIntegral a b accuracy (sum ys)

-- | Takes a range, n and seed for randomness.
-- Returns n random numbers from a range.
randomX :: Double -> Double -> Int -> Int -> [Double]
randomX a b n seed = do
  let g = mkStdGen seed
  take n $ randomRs (a, b) g

-- | Takes the interval of integration and the number of points.
-- Parallel calculates the integral calculated by the Monte Carlo method.
parallelIntegrate :: Double -> Double -> Int -> Double
parallelIntegrate a b accuracy =
  let sumN = runPar $ do
        let range = InclusiveRange 0 (accuracy - 1)
        let mapper x =
              return $ fun $ (head $ randomX a b 1 (239 * x))
        let reducer x y = return (x + y)
        parMapReduceRange range mapper reducer 0
  in  monteCarloIntegral a b accuracy sumN