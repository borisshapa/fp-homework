module Task3Spec
  ( spec
  ) where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.Async (runConcurrently, Concurrently(..))
import Control.Exception.Base (AsyncException(..), throw, catch)
import Control.Monad (forM_)

import Task3.ConcurrentHashTable
import Task3.ConcurrentHashTableUtils

import Test.Hspec

checkN :: Int -> ConcurrentHashTable String Int -> IO ()
checkN n cht =
  forM_ (list1_n n) $ \ind -> do
    val <- getCHT ("key" ++ show ind) cht
    val `shouldBe` Just ind

spec :: Spec
spec = do
  describe "newCHT" $ do
    it "newCHT" $ do
      cht <- newCHT
      size <- sizeCHT cht
      size `shouldBe` 0

  describe "putCHT getCHT" $ do
    it "check 1 element" $ do
      cht <- newCHT
      putCHT "key" (1 :: Int) cht
      val <- getCHT "key" cht
      val `shouldBe` Just 1
    it "check 100 elements" $ do
      cht <- newCHT
      putN 100 cht
      checkN 100 cht
    it "get Nothing" $ do
      cht <- newCHT
      putN 100 cht
      val <- getCHT "key100000" cht
      val `shouldBe` Nothing

  describe "sizeCHT" $ do
    it "check 1 element" $ do
      cht <- newCHT
      putCHT "key" (1 :: Int) cht
      size <- sizeCHT cht
      size `shouldBe` 1
    it "check 100 elements" $ do
      cht <- newCHT
      putN 100 cht
      size <- sizeCHT cht
      size `shouldBe` 100

  describe "multithreaded execution" $ do
    it "check 100 elements" $ do
      cht <- newCHT
      _ <- runConcurrently $ (,)
        <$> Concurrently (putA_B 1 50 cht)
        <*> Concurrently (putA_B 51 100 cht)
      checkN 100 cht
    it "put and get in diffrent threads" $ do
      cht <- newCHT
      _ <- runConcurrently2Threads (100 :: Int) putN getN cht
      size <- sizeCHT cht
      size `shouldBe` 100

    it "throw exception" $ do
      cht <- newCHT
      catch (do
        _ <- runConcurrently $ (,)
          <$> Concurrently (putN 100000 cht)
          <*> Concurrently (threadDelay 500000 >> throw ThreadKilled)
        return ())
            (\(ThreadKilled) -> return ())
      size <- sizeCHT cht
      putCHT "key" 0 cht
      newSize <- sizeCHT cht
      newSize `shouldBe` size + 1

    it "throwTo exception" $ do
      cht <- newCHT
      threadId <- forkIO (putN 100000 cht)
      _ <- forkIO (threadDelay 500000 >> killThread threadId)
      size <- sizeCHT cht
      putCHT "key" 0 cht
      newSize <- sizeCHT cht
      newSize `shouldBe` size + 1