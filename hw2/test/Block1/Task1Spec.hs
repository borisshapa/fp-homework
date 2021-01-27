{-# LANGUAGE ScopedTypeVariables #-}

module Block1.Task1Spec
  (
    spec
  ) where

import Data.List (intercalate)

import Block1.Task1

import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "stringSum" $ do
    it "1 2 3" $ do
      (stringSum "1 2 3") `shouldBe` Just 6
    it "1 a 3" $ do
      (stringSum "1 a 3") `shouldBe` Nothing
    it "1 10 15 2 with spaces" $ do
      (stringSum "1             \n 10 \t 15    2 ") `shouldBe` Just 28
    it "1 10 a 2 with spaces" $ do
      (stringSum "1             \n 10 \t a    2") `shouldBe` Nothing
    it "empty string" $ do
      (stringSum "") `shouldBe` Just 0
    it "one integer string property" $ property $ do
      \(x :: Int) -> (stringSum (show x)) `shouldBe` Just x
    it "list string property" $ property $ do
      \(xs :: [Int]) -> (stringSum (intercalate " " (map show xs)))
        `shouldBe` Just (sum xs)