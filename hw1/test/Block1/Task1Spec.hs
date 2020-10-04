module Block1.Task1Spec
  (
    spec
  ) where

import Block1.Task1

import Test.Hspec

spec :: Spec
spec = do
  describe "nextDay" $ do
    it "Monday -> Tuesday" $ do
      nextDay Monday `shouldBe` Tuesday

    it "Tuesday -> Wednesday" $ do
      nextDay Tuesday `shouldBe` Wednesday

    it "Wednesday -> Thursday" $ do
      nextDay Wednesday `shouldBe` Thursday

    it "Thursday -> Friday" $ do
      nextDay Thursday `shouldBe` Friday

    it "Friday -> Saturday" $ do
      nextDay Friday `shouldBe` Saturday

    it "Saturday -> Sunday" $ do
      nextDay Saturday `shouldBe` Sunday

    it "Sunday -> Monday" $ do
      nextDay Sunday `shouldBe` Monday

  describe "afterDays" $ do
    it "Monday 0 days after Monday" $ do
      afterDays Monday 0 `shouldBe` Monday

    it "Friday 4 days after Monday" $ do
      afterDays Monday 4 `shouldBe` Friday

    it "Tuesday 7 days after Tuesday" $ do
      afterDays Tuesday 7 `shouldBe` Tuesday

    it "Wednesday 14 days after Wednesday" $ do
      afterDays Wednesday 14 `shouldBe` Wednesday

    it "Tuesday 25 days after Friday" $ do
      afterDays Friday 25 `shouldBe` Tuesday

    it "Sunday 365 days after Saturday" $ do
      afterDays Saturday 365 `shouldBe` Sunday

  describe "isWeekend" $ do
    it "Monday isn't weekend" $ do
      isWeekend Monday `shouldBe` False

    it "Tuesday isn't weekend" $ do
      isWeekend Tuesday `shouldBe` False

    it "Wednesday isn't weekend" $ do
      isWeekend Wednesday `shouldBe` False

    it "Thursday isn't weekend" $ do
      isWeekend Thursday `shouldBe` False

    it "Friday isn't weekend" $ do
      isWeekend Friday `shouldBe` False

    it "Saturday is weekend" $ do
      isWeekend Saturday `shouldBe` True

    it "Sunday is weekend" $ do
      isWeekend Sunday `shouldBe` True

  describe "daysToParty" $ do
    it "Friday 4 days after Monday" $ do
      daysToParty Monday `shouldBe` 4

    it "Friday 3 days after Tuesday" $ do
      daysToParty Tuesday `shouldBe` 3

    it "Friday 2 days after Wednesday" $ do
      daysToParty Wednesday `shouldBe` 2

    it "Friday 1 day after Thursday" $ do
      daysToParty Thursday `shouldBe` 1

    it "Friday 0 days after Friday" $ do
      daysToParty Friday `shouldBe` 0

    it "Friday 6 days after Saturday" $ do
      daysToParty Saturday `shouldBe` 6

    it "Friday 5 days after Sunday" $ do
      daysToParty Sunday `shouldBe` 5