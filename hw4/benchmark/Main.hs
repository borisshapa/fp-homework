module Main
  ( main
  ) where

import Task1Bench
import Task2Bench
import Task3Bench

main :: IO ()
main = do
  Task1Bench.benchmark
  Task2Bench.benchmark
  Task3Bench.benchmark