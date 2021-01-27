module Block3.Task2Spec
  (
    spec
  ) where

import Control.Applicative (Alternative, (<|>))
import Data.Char (isDigit)

import Block3.Task1 (Parser(..))
import Block3.Task2

import Test.Hspec

spec :: Spec
spec = do
  describe "ok" $ do
    it "ok" $ do
      runParser ok "hello, my friend" `shouldBe` Just ((), "hello, my friend")

  describe "eof" $ do
    it "eof" $ do
      runParser eof "" `shouldBe` Just ((), "")
    it "not eof" $ do
      runParser eof "hello, my friend" `shouldBe` Nothing

  describe "satisfy" $ do
    it "is Digit true" $ do
      runParser (satisfy isDigit) "123" `shouldBe` Just ('1', "23")
    it "is Digit false" $ do
      runParser (satisfy isDigit) "abc" `shouldBe` Nothing

  describe "element" $ do
    it "element a" $ do
      runParser (element 'a') "abc" `shouldBe` Just ('a', "bc")
    it "not element a" $ do
      runParser (element 'a') "bca" `shouldBe` Nothing

  describe "stream" $ do
    it "abc" $ do
      runParser (stream "abc") "abcabc" `shouldBe` Just ("abc", "abc")
    it "not abc" $ do
      runParser (stream "abc") "abacaba" `shouldBe` Nothing