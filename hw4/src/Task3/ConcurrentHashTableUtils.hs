module Task3.ConcurrentHashTableUtils
  ( -- * functions for easy use and testing ConcurrentHashTable
    getN
  , list1_n 
  , putA_B
  , putN
  , runConcurrently2Threads
  , runConcurrently4Threads
  , sizeN
  ) where

import Control.Monad (forM_)
import Control.Monad (replicateM_)
import Control.Concurrent.Async (Concurrently(..), runConcurrently)

import Task3.ConcurrentHashTable

-- | [a, a + 1, a + 2, ... , b - 1, b]
listA_B :: Int -> Int -> [Int]
listA_B a b = take (b - a + 1) $ iterate (+1) a

-- | [1, 2, 3, ... , n - 1, n]
list1_n :: Int -> [Int]
list1_n n = listA_B 1 n

-- | Puts to ConcurrentHashTable the following pairs ("keyN", N) for N in [a..b]
putA_B :: Int -> Int -> ConcurrentHashTable String Int -> IO ()
putA_B a b cht =
  forM_ (listA_B a b) $ \ind -> putCHT ("key" ++ (show ind)) ind cht

-- | Puts to ConcurrentHashTable n times.
putN :: Int -> ConcurrentHashTable String Int -> IO ()
putN n cht = putA_B 1 n cht

-- | Gets from ConcurrentHashTable n times.
getN :: Int -> ConcurrentHashTable String Int -> IO ()
getN n cht = forM_ (list1_n n) $ \ind -> getCHT ("key" ++ (show ind)) cht

-- | Recieve ConcurrentHashtable size n times
sizeN :: Int -> ConcurrentHashTable String Int -> IO ()
sizeN n cht = replicateM_ n (sizeCHT cht)

-- | Runs operations on ConcurrentHashTable in 2 threads. n operations each.
runConcurrently2Threads
  :: Int
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> ConcurrentHashTable String Int
  -> IO ()
runConcurrently2Threads opsPerThread fun1 fun2 cht = do
  _ <- runConcurrently $ (,)
    <$> Concurrently (fun1 opsPerThread cht)
    <*> Concurrently (fun2 opsPerThread cht)
  return ()

-- | Runs operations on ConcurrentHashTable in 4 threads. n operations each.
runConcurrently4Threads
  :: Int
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> (Int -> ConcurrentHashTable String Int -> IO ())
  -> ConcurrentHashTable String Int
  -> IO ()
runConcurrently4Threads opsPerThread fun1 fun2 cht = do
  _ <- runConcurrently $ (,,,)
    <$> Concurrently (fun1 opsPerThread cht)
    <*> Concurrently (fun1 opsPerThread cht)
    <*> Concurrently (fun2 opsPerThread cht)
    <*> Concurrently (fun2 opsPerThread cht)
  return ()