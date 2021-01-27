module Block2.Task1Spec
  (
    spec
  ) where

import Block2.Task1

import Test.Hspec

spec :: Spec
spec = do
  describe "Expression" $ do
    it "add" $ do
      eval (Add (Const 1) (Const 3)) `shouldBe` Right 4
    it "sub" $ do
      eval (Subtract (Const 3) (Const 1)) `shouldBe` Right 2
    it "multiply" $ do
      eval (Multiply (Const 2) (Const 4)) `shouldBe` Right 8
    it "div" $ do
      eval (Divide (Const 10) (Const 2)) `shouldBe` Right 5
    it "pow" $ do
      eval (Pow (Const 3) (Const 3)) `shouldBe` Right 27
    it "div by zero" $ do
      eval (Divide (Const 5) (Const 0)) `shouldBe` Left DivisionByZero
    it "negative degree" $ do
      eval (Pow (Const 5) (Const (-1))) `shouldBe` Left NegativeDegree
    it "expression" $ do
      eval (Add (Multiply (Pow (Const 2) (Const 4))
        (Subtract (Const 5) (Const 3))) (Divide (Const 24) (Const 5)))
      `shouldBe` Right 36