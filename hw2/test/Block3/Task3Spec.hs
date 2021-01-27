module Block3.Task3Spec
  (
    spec
  ) where

import Block3.Task1 (Parser(..))
import Block3.Task2 (eof)
import Block3.Task3

import Test.Hspec

spec :: Spec
spec = do
  describe "parenthes" $ do
    let parse = runParser (parenthesesParser <* eof)
    let ok = Just ((), "")
    let failed = Nothing
    it "()" $ do
      parse "()" `shouldBe` ok
    it "()()" $ do
      parse "()()" `shouldBe` ok
    it "(()())()((()))" $ do
      parse "(()())()((()))" `shouldBe` ok
    it ")" $ do
      parse ")" `shouldBe` failed
    it "(()()" $ do
      parse "(()()" `shouldBe` failed

  describe "integer" $ do
    let parse = runParser (integerParser <* eof)
    let failed = Nothing
    it "123" $ do
      parse "123" `shouldBe` Just (123, "")
    it "-123" $ do
      parse "-123" `shouldBe` Just (-123, "")
    it "+123" $ do
      parse "+123" `shouldBe` Just (123, "")
    it "+1two2 (error)" $ do
      parse "+1two3" `shouldBe` failed