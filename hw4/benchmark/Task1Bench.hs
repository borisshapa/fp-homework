module Task1Bench
  ( benchmark
  ) where

import Task1.Lazy
import Task1.Strict

import Criterion.Main

trianglePolygon :: (Int -> Int -> a) -> Int -> [a]
trianglePolygon constructor cnt =
  constructor 0 1 : [constructor x1 0 | x1 <- [0..cnt]]

strictTrianglePolygon :: Int -> [Point]
strictTrianglePolygon = trianglePolygon Point

lazyTrianglePolygon :: Int -> [LazyPoint]
lazyTrianglePolygon = trianglePolygon LazyPoint

benchmark :: IO ()
benchmark = defaultMain
  [ bgroup "perimeter" 
    [ bench "10 ^ 3" $
        nf perimeter (strictTrianglePolygon $ 10 ^ (3 :: Int))
    , bench "10 ^ 5" $
        nf perimeter (strictTrianglePolygon $ 10 ^ (5 :: Int))
    , bench "10 ^ 7" $
        nf perimeter (strictTrianglePolygon $ 10 ^ (7 :: Int))
    ]
  , bgroup "slow perimeter"
    [ bench "10 ^ 3" $
        nf slowPerimeter (lazyTrianglePolygon $ 10 ^ (3 :: Int))
    , bench "10 ^ 5" $
        nf slowPerimeter (lazyTrianglePolygon $ 10 ^ (5 :: Int))
    , bench "10 ^ 7" $
        nf slowPerimeter (lazyTrianglePolygon $ 10 ^ (7 :: Int))
    ]
  , bgroup "doubleArea"
    [ bench "10 ^ 3" $
        nf doubleArea (strictTrianglePolygon $ 10 ^ (3 :: Int))
    , bench "10 ^ 5" $
        nf doubleArea (strictTrianglePolygon $ 10 ^ (5 :: Int))
    , bench "10 ^ 7" $
        nf doubleArea (strictTrianglePolygon $ 10 ^ (7 :: Int))
    ]
  , bgroup "slow doubleArea"
    [ bench "10 ^ 3" $
        nf slowDoubleArea (lazyTrianglePolygon $ 10 ^ (3 :: Int))
    , bench "10 ^ 5" $
        nf slowDoubleArea (lazyTrianglePolygon $ 10 ^ (5 :: Int))
    , bench "10 ^ 7" $
        nf slowDoubleArea (lazyTrianglePolygon $ 10 ^ (7 :: Int))
    ]
  ]