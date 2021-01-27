module Block3.Task4Spec
  (
    spec
  ) where

import Block3.Task1 (Parser(..))
import Block3.Task4

import Test.Hspec

spec :: Spec
spec = do
  describe "listlistParser" $ do
    let parse = runParser listlistParser
    it "0" $ do
      parse "0" `shouldBe` Just ([[]], "")
    it "1, 2" $ do
      parse "1, 2" `shouldBe` Just ([[2]], "")
    it "3, 10, 20, -30" $ do
      parse "3, 10, 20, -30" `shouldBe` Just ([[10, 20, -30]], "")
    it "3, 10, 20, -30, 2, 4, 5, 0, 1, -100" $ do
      parse "3, 10, 20, -30, 2, 4, 5, 0, 1, -100"
      `shouldBe` Just ([[10, 20, -30], [4, 5], [], [-100]], "")
    it "       1,                  2    ,  3   , -1, -2,   3  " $ do
      parse "       1,                  2    ,  3   , -1, -2,   3  "
      `shouldBe` Just ([[2], [-1, -2, 3]], "")
    it "1 , ," $ do
      parse "1 , ," `shouldBe` Nothing
    it "2, abc, 1" $ do
      parse "2, abc, 1" `shouldBe` Nothing
    it "3, 1, 2" $ do
      parse "3, 1, 2" `shouldBe` Nothing
    it "empty input" $ do
      parse "" `shouldBe` Just ([], "")