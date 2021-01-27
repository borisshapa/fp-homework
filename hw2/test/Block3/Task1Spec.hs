module Block3.Task1Spec
  (
    spec
  ) where

import Control.Applicative (Alternative, (<|>))

import Block3.Task1
import Block3.Task2 (element)

import Test.Hspec

spec :: Spec
spec = do
  let parser = (element 'a')
  describe "functor" $ do
    it "fmap id = id" $ do
      runParser (fmap id parser) "ab" `shouldBe` runParser (id parser) "ab"
    it "fmap (f . g) = fmap f . fmap g" $ do
      runParser ((fmap ((+ (1 :: Int)) . (+ (2 :: Int)))) (element 1)) [1, 2]
        `shouldBe` runParser ((fmap (+ (1 :: Int)) . fmap (+ (2 :: Int)))
          (element 1)) [1, 2]

  describe "applicative" $ do
    it "pure id <*> v = v" $ do
      runParser (pure id <*> parser) "ab" `shouldBe` runParser parser "ab"
    it "pure f <*> pure x = pure (f x)" $ do
      runParser (pure (+ 1) <*> pure 2) [2, 3]
        `shouldBe` runParser (pure ((+ 1) 2)) [2, 3]
    it "builder" $ do
      runParser ((+) <$> element 1 <*> element 2) [1, 2] `shouldBe` Just (3, [])

  describe "monad" $ do
    it "do block" $ do
      runParser (do
        a <- element 'a'
        b <- element 'b'
        return ([a, b])
        ) "ab"
      `shouldBe` Just ("ab", [])

  describe "alternative" $ do
    it "choosing a" $ do
      runParser (element 'a' <|> element 'b') "ab" `shouldBe` Just ('a', "b")
    it "choosing b" $ do
      runParser (element 'a' <|> element 'b') "ba" `shouldBe` Just ('b', "a")
    it "choosing nothing" $ do
      runParser (element 'a' <|> element 'b') "ca" `shouldBe` Nothing