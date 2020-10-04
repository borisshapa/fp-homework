{-# LANGUAGE RankNTypes #-}

module Block1.Task2Spec
  (
    spec
  ) where

import Block1.Task2

import Control.Exception (evaluate)
import Test.Hspec

type Assertion = IO ()

-- | Takes an operation on numbers.
-- Applies it to integers and natural numbers,
-- compares the result.
testNatOperation
  :: (forall a. Integral a => (a -> a -> a)) -> Int -> Int -> Assertion
testNatOperation operation int1 int2 =
  natToInt (operation nat1 nat2) `shouldBe` (max (operation int1 int2) 0)
  where
    nat1 = intToNat int1
    nat2 = intToNat int2

-- | Compares the result of summation of integers and natural numbers.
testNatSum :: Int -> Int -> Assertion
testNatSum = testNatOperation (+)

-- | Compares the result of subtraction of integers and natural numbers.
testNatSub :: Int -> Int -> Assertion
testNatSub = testNatOperation (-)

-- | Compares the result of multiplication of integers and natural numbers.
testNatMul :: Int -> Int -> Assertion
testNatMul = testNatOperation (*)

-- | Compares the result of division of integers and natural numbers.
testNatDiv :: Int -> Int -> Assertion
testNatDiv = testNatOperation div

-- | Compares the result of getting remainder of integers and natural numbers.
testNatMod :: Int -> Int -> Assertion
testNatMod = testNatOperation mod

-- | Takes an comparison operation on numbers.
-- Applies it to integers and natural numbers,
-- compares the result.
testNatComparison
  :: (forall a. Ord a => (a -> a -> Bool)) -> Int -> Int -> Assertion
testNatComparison cmp int1 int2 = (cmp nat1 nat2) `shouldBe` (cmp int1 int2)
  where
    nat1 = intToNat int1
    nat2 = intToNat int2

-- | Compares the result of a comparison operation '=='
-- for integers and natural numbers.
testNatEq :: Int -> Int -> Assertion
testNatEq = testNatComparison (==)

-- | Compares the result of a comparison operation '>'
-- for integers and natural numbers.
testNatGreater :: Int -> Int -> Assertion
testNatGreater = testNatComparison (>)

-- | Compares the result of a comparison operation '<'
-- for integers and natural numbers.
testNatLess :: Int -> Int -> Assertion
testNatLess = testNatComparison (<)

-- | Compares the result of a comparison operation '>='
-- for integers and natural numbers.
testNatGE :: Int -> Int -> Assertion
testNatGE = testNatComparison (>=)

-- | Compares the result of a comparison operation '<='
-- for integers and natural numbers.
testNatLE :: Int -> Int -> Assertion
testNatLE = testNatComparison (<=)

spec :: Spec
spec = do
  describe "intToNat" $ do
    it "0 == Z" $ do
      intToNat 0 `shouldBe` Z
    it "1 == S Z" $ do
      intToNat 1 `shouldBe` (S Z)
    it "2 == (S (S Z))" $ do
      intToNat 2 `shouldBe` (S (S Z))
    it "7 == (S (S (S (S (S (S (S Z)))))))" $ do
      intToNat 7 `shouldBe` (S (S (S (S (S (S (S Z)))))))
    it "11 == (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))" $ do
      intToNat 11 `shouldBe` (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))

  describe "natToInt" $ do
    it "Z == 0" $ do
      natToInt Z `shouldBe` 0
    it "S Z == 1" $ do
      natToInt (S Z) `shouldBe` 1
    it "(S (S Z)) == 2" $ do
      natToInt (S (S Z)) `shouldBe` 2
    it "(S (S (S (S (S Z))))) == 3" $ do
      natToInt (S (S (S (S (S Z))))) `shouldBe` 5
    it "(S (S (S (S (S (S (S (S (S (S Z)))))))))) == 10" $ do
      natToInt (S (S (S (S (S (S (S (S (S (S Z)))))))))) `shouldBe` 10

  describe "+" $ do
    it "2 + 2 == 4" $ do
      testNatSum 2 2
    it "14 + 0 == 14" $ do
      testNatSum 14 0
    it "0 + 53 == 53" $ do
      testNatSum 0 53
    it "364 + 1 == 365" $ do
      testNatSum 364 1
    it "2 + 4 == 6" $ do
      testNatSum 2 4
    it "14 + 24 == 38" $ do
      testNatSum 14 24
    it "97 + 54 == 151" $ do
      testNatSum 97 54

  describe "-" $ do
    it "2 - 2 == 0" $ do
      testNatSub 2 2
    it "0 - 1024 == 0 (in natural)" $ do
      testNatSub 0 1024
    it "1 - 1023 == 0 (in natural)" $ do
      testNatSub 1 1023
    it "239 - 1 == 238" $ do
      testNatSub 239 1
    it "239 - 0 == 239" $ do
      testNatSub 239 0
    it "1024 - 512 == 512" $ do
      testNatSub 1024 512

  describe "*" $ do
    it "2 * 2 == 4" $ do
      testNatMul 2 2
    it "0 * 1024 == 0" $ do
      testNatMul 0 1024
    it "1024 * 0 == 0" $ do
      testNatMul 1024 0
    it "1 * 432 == 432" $ do
      testNatMul 1 432
    it "432 * 1 == 432" $ do
      testNatMul 432 1
    it "42 * 17 == 714" $ do
      testNatMul 42 17

  describe "/" $ do
    it "2 / 2 == 1" $ do
      testNatDiv 2 2
    it "21 / 3 == 7" $ do
      testNatDiv 21 3
    it "113 / 7 == 16" $ do
      testNatDiv 113 7
    it "0 / 13 == 0" $ do
      testNatDiv 0 13
    it "13 / 1 == 13" $ do
      testNatDiv 13 1

  describe "%" $ do
    it "2 % 2 == 0" $ do
      testNatMod 2 2
    it "13 % 26 == 13" $ do
      testNatMod 13 26
    it "12 % 1 == 0" $ do
      testNatMod 12 1
    it "0 % 12 == 0" $ do
      testNatMod 0 12
    it "101 % 7 == 3" $ do
      testNatMod 101 7

  describe "==" $ do
    it "2 == 2" $ do
      testNatEq 2 2
    it "0 == 0" $ do
      testNatEq 0 0
    it "0 != 112" $ do
      testNatEq 0 112
    it "112 != 0" $ do
      testNatEq 112 0
    it "112 != 121" $ do
      testNatEq 112 121

  describe "<" $ do
    it "2 !< 2" $ do
      testNatLess 2 2
    it "0 < 2" $ do
      testNatLess 0 2
    it "2 !< 0" $ do
      testNatLess 2 0
    it "112 < 121" $ do
      testNatLess 112 121
    it "1021 !< 1" $ do
      testNatLess 1021 1

  describe "<=" $ do
    it "2 <= 2" $ do
      testNatLE 2 2
    it "0 <= 2" $ do
      testNatLE 0 2
    it "2 !<= 2" $ do
      testNatLE 2 0
    it "113 !<= 112" $ do
      testNatLE 113 112
    it "0 <= 0" $ do
      testNatLE 0 0

  describe ">" $ do
    it "2 !> 2" $ do
      testNatGreater 2 2
    it "0 !> 2" $ do
      testNatGreater 0 2
    it "2 > 0" $ do
      testNatGreater 2 0
    it "113 > 112" $ do
      testNatGreater 113 112
    it "0 !> 0" $ do
      testNatGreater 0 0

  describe ">=" $ do
    it "2 >= 2" $ do
      testNatGE 2 2
    it "0 !>= 2" $ do
      testNatGE 0 2
    it "2 >= 0" $ do
      testNatGE 2 0
    it "114 >= 113" $ do
      testNatGE 114 113
    it "0 >= 0" $ do
      testNatGE 0 0

  describe "% 2" $ do
    it "0 is even" $ do
      (isEven (intToNat 0)) `shouldBe` True
    it "1 is odd" $ do
      (isEven (intToNat 1)) `shouldBe` False
    it "512 is even" $ do
      (isEven (intToNat 512)) `shouldBe` True
    it "239 is odd" $ do
      (isEven (intToNat 239)) `shouldBe` False

  describe "division by zero" $ do
    it "there must be an error when dividing" $ do
      evaluate ((S Z) `div` Z) `shouldThrow` errorCall "division by zero"
    it "there must be an error when getting remainder" $ do
      evaluate ((S (S (S (S (S Z))))) `mod` Z)
        `shouldThrow` errorCall "division by zero"

  describe "negative number" $ do
    it "there must be an error when converting negative number to natural" $ do
      evaluate (intToNat (-133))
        `shouldThrow` errorCall "natural number expected"