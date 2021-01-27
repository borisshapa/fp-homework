module Task3Bench
  ( benchmark
  ) where

import Task3.ConcurrentHashTable
import Task3.ConcurrentHashTableUtils

import Criterion.Main

testSimpleOperation ::
  Int -> (Int -> ConcurrentHashTable String Int -> IO ()) -> Benchmarkable
testSimpleOperation n fun = nfIO $ do
  cht <- newCHT
  fun n cht

testPut :: Int -> Benchmarkable
testPut n = testSimpleOperation n putN

testPutGet :: Int -> Int -> Benchmarkable
testPutGet nPut nGet = nfIO $ do
  cht <- newCHT
  putN nPut cht
  getN nGet cht

testConcurrently2Threads
  :: Int
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> Benchmarkable
testConcurrently2Threads opsPerThread fun1 fun2 =
  nfIO $ do
    cht <- newCHT
    runConcurrently2Threads opsPerThread fun1 fun2 cht

testConcurrently4Threads
  :: Int
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> Benchmarkable
testConcurrently4Threads opsPerThread fun1 fun2 =
  nfIO $ do
    cht <- newCHT
    runConcurrently4Threads opsPerThread fun1 fun2 cht

benchmark :: IO ()
benchmark = defaultMain
  [ bgroup "1 thread" $
      let opsPerThread10_3 = 10 ^ (3 :: Int)
          opsPerThread10_5 = 10 ^ (5 :: Int)
      in
        [ bench "put x 10 ^ 3" $ testPut (10 ^ (3 :: Int))
        , bench "put x 10 ^ 5" $ testPut (10 ^ (5 :: Int))
        
        , bench "put and get x 10 ^ 3" $
            testPutGet (opsPerThread10_3 `div` 2) (opsPerThread10_3 `div` 2)
        , bench "put and get x 10 ^ 5" $
            testPutGet (opsPerThread10_5 `div` 2) (opsPerThread10_5 `div` 2)
        ]

  , bgroup "2 threads" $
      let opsPerThread10_3 = 10 ^ (3 :: Int) `div` 2
          opsPerThread10_5 = 10 ^ (5 :: Int) `div` 2
      in
        [ bench "put x 10 ^ 3" $
            testConcurrently2Threads opsPerThread10_3 putN putN
        , bench "put x 10 ^ 5" $
            testConcurrently2Threads opsPerThread10_5 putN putN

        , bench "put and get in different threads x 10 ^ 3" $
            testConcurrently2Threads opsPerThread10_3 putN getN
        , bench "put and get in different threads x 10 ^ 5" $
            testConcurrently2Threads opsPerThread10_5 putN getN

        , bench "put and size in different threads x 10 ^ 3" $
            testConcurrently2Threads opsPerThread10_3 putN sizeN
        , bench "put and size in different threads x 10 ^ 5" $
            testConcurrently2Threads opsPerThread10_5 putN sizeN
        ]

  , bgroup "4 threads" $
      let opsPerThread10_3 = 10 ^ (3 :: Int) `div` 4
          opsPerThread10_5 = 10 ^ (5 :: Int) `div` 4
      in
        [ bench "put x 10 ^ 3" $
            testConcurrently4Threads opsPerThread10_3 putN putN
        , bench "put x 10 ^ 5" $
            testConcurrently4Threads opsPerThread10_5 putN putN

        , bench "put and get in different threads x 10 ^ 3" $
            testConcurrently4Threads opsPerThread10_3 putN getN
        , bench "put and get in different threads x 10 ^ 5" $
            testConcurrently4Threads opsPerThread10_5 putN getN

        , bench "put and size in different threads x 10 ^ 3" $
            testConcurrently4Threads opsPerThread10_3 putN sizeN
        , bench "put and size in different threads x 10 ^ 5" $
            testConcurrently4Threads opsPerThread10_5 putN sizeN
        ]
  ]