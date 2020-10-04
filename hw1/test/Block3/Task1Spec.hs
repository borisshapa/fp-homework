module Block3.Task1Spec
  (
    spec
  ) where

import Data.Monoid(Sum(..))

import Block3.Task1

import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "maybeConcat" $ do
    it "maybe concat" $ do
      maybeConcat [Just [1,2,3], Nothing, Just [4,5]]
        `shouldBe` [1, 2, 3, 4, 5]

    it "maybe concat of list with only nothing" $ do
      maybeConcat [Nothing, Nothing, Nothing] `shouldBe` ""

    it "maybe concat of empty list" $ do
      maybeConcat ([] :: [Maybe [Integer]]) `shouldBe` []

  describe "eitherConcat" $ do
    it "either concat" $ do
      eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
        `shouldBe` (Sum 8, [1, 2, 3, 4, 5])

    it "either concat (left right swap)" $ do
      eitherConcat [Right (Sum 3), Left [1,2,3], Right (Sum 5), Left [4,5]]
        `shouldBe` ([1, 2, 3, 4, 5], Sum 8)

    it "either concat of list with only Right" $ do
      eitherConcat [Right [1,2,3], Right [4,5]]
        `shouldBe` ("", [1, 2, 3, 4, 5])

    it "either concat of list with only Left" $ do
      eitherConcat [Left (Sum 3), Left (Sum 5)] `shouldBe` (Sum 8, "")

    it "either concat of empty list" $ do
      eitherConcat [] `shouldBe` ("", "")