{-# LANGUAGE RankNTypes #-}

module Block3.Task2Spec
  (
    spec
  ) where

import Block3.Task2

import Test.Hspec

-- | The type of check for testing.
type Assertion = IO ()

-- | Checking the associativity of a semigroup operation.
testAssociative
  :: forall a. (Semigroup a, Eq a, Show a)
  => a -> a -> a -> Assertion
testAssociative a b c = (a <> (b <> c)) `shouldBe` ((a <> b) <> c)

spec :: Spec
spec = do
  describe "NonEmpty" $ do
    it "Semigroup operation for NonEmptys with only one elements" $ do
      (1 :| []) <> (2 :| []) `shouldBe` (1 :| [2])

    it "Semigroup operation for NonEmptys" $ do
      (1 :| [2]) <> (3 :| [4, 5]) `shouldBe` (1 :| [2, 3, 4, 5])

    it "Semigroup operation for non empty strings" $ do
      ('h' :| "ello") <> (' ' :| "world") `shouldBe` ('h' :| "ello world")

    it "associative of Semigroup operation" $ do
      testAssociative (1 :| []) (2 :| [3]) (4 :| [5])

  describe "ThisOrThat" $ do
    it "This 'a' <> That 1 == Both 'a' 1" $ do
      (This 'a') <> (That 1) `shouldBe` Both 'a' 1

    it "This 1 <> This 2 == This 2" $ do
      (This 1) <> (This 2) `shouldBe` (This 2 :: ThisOrThat Int Int)

    it "That 'a' <> That 'b' == That 'b'" $ do
      (That 'a') <> (That 'b') `shouldBe` (That 'b' :: ThisOrThat Char Char)

    it "This 'c' <> That 'd' <> Both 'a' 'b' == Both 'a' 'b'" $ do
      (This 'c') <> (That 'd') <> (Both 'a' 'b') `shouldBe` Both 'a' 'b'

    it "That 'b' <> This 'c' <> This 'd' == Both 'd' 'b'" $ do
      (That 'b') <> (This 'c') <> (This 'd') `shouldBe` Both 'd' 'b'

    it "associative this that this" $ do
      testAssociative (This 'a') (That 'b') (This 'a')

    it "associative this this this" $ do
      testAssociative (This 'a' :: ThisOrThat Char Char) (This 'b') (This 'c')

    it "associative both this that" $ do
      testAssociative (Both 'a' 'b') (This 'c') (That 'd')

    it "associative this both that" $ do
      testAssociative (This 'a') (Both 'c' 'd') (That 'b')

    it "associative that that both" $ do
      testAssociative (That 'b') (That 'a') (Both 'e' 'b')

  describe "string concatenation" $ do
    it "Semigroup operation for Name" $ do
      Name "root" <> Name "server" `shouldBe` Name "root.server"

    it "Semigroup operation fith mempty" $ do
      Name "root" <> mempty `shouldBe` Name "root"

    it "mempty <> name" $ do
      mempty <> Name "server" `shouldBe` Name "server"

    it "associative name name name" $ do
      testAssociative (Name "root") (Name "local") (Name "server")

    it "associative mempty name name" $ do
      testAssociative mempty (Name "local") (Name "server")

    it "associative name mempty name" $ do
      testAssociative (Name "root") mempty (Name "server")

    it "associative name name mempty" $ do
      testAssociative (Name "root") (Name "local") mempty

  describe "Endo" $ do
    it "Semigroup operation for Endo" $ do
     getEndo (Endo (2 +) <> Endo (3 +)) 2 `shouldBe` 7

    it "Semigroup operation for Endo with mempty" $ do
      getEndo (Endo (2 +) <> mempty) 2 `shouldBe` getEndo (Endo (2 +)) 2

    it "Endo for concatination" $ do
      getEndo (Endo ((++) "pref") <> Endo ((++) "ix")) "suffix"
        `shouldBe` "prefixsuffix"

    it "associative" $ do
      getEndo (Endo (2 +) <> (Endo (3 +) <> Endo (4 +))) 2
        `shouldBe` getEndo ((Endo (2 +) <> Endo (3 +)) <> Endo (4 +)) 2