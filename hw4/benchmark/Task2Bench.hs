module Task2Bench
  ( benchmark
  ) where

import Data.Tuple.Extra (uncurry3)

import Task2.MonteCarlo

import Criterion.Main

benchmark :: IO ()
benchmark = defaultMain
  [ bgroup "consistent integrate from 0.1 to 10.25" 
    [ bench "N = 10 ^ 2" $
        nf integrateUncurry (0.1, 10.25, 10 ^ (2 :: Int))
    , bench "N = 10 ^ 4" $
        nf integrateUncurry (0.1, 10.25, 10 ^ (4 :: Int))
    , bench "N = 10 ^ 6" $
        nf integrateUncurry (0.1, 10.25, 10 ^ (6 :: Int))
    ]
  , bgroup "parallel integrate from 0.1 to 10.25"
    [ bench "N = 10 ^ 2" $
        nf parallelIntegrateUncurry (0.1, 10.25, 10 ^ (2 :: Int))
    , bench "N = 10 ^ 4" $
        nf parallelIntegrateUncurry (0.1, 10.25, 10 ^ (4 :: Int))
    , bench "N = 10 ^ 6" $
        nf parallelIntegrateUncurry (0.1, 10.25, 10 ^ (6 :: Int))
    ]
  ]
    where
      integrateUncurry = uncurry3 integrate
      parallelIntegrateUncurry = uncurry3 parallelIntegrate