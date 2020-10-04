module Block2.Task2Spec
  (
    spec
  ) where

import Data.List.NonEmpty (NonEmpty (..))

import Block2.Task2

import Test.Hspec

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "split 'path/to/file' by '/'" $ do
      splitOn '/' "path/to/file" `shouldBe` ("path" :| ["to", "file"])

    it "split 'path//to//file' by '/'" $ do
      splitOn '/' "path//to//file" `shouldBe`
        ("path" :| ["", "to", "", "file"])

    it "split '' by '/'" $ do
      splitOn '/' "" `shouldBe` ("" :| [])

    it "split 'hello' by 's'" $ do
      splitOn 's' "hello" `shouldBe` ("hello" :| [])

    it "split 'success' by 's'" $ do
      splitOn 's' "success" `shouldBe` ("" :| ["ucce", "", ""])

    it "split [2, 3, 4, 1, 3, 10, 1, 5] by 1" $ do
      splitOn 1 [2, 3, 4, 1, 3, 10, 1, 5] `shouldBe`
        ([2, 3, 4] :| [[3, 10], [5]])

  describe "joinWith" $ do
    it "join ['path', 'to', 'file'] with '/'" $ do
      joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"

    it "join ['path', 'to', '', 'file'] with '/'" $ do
      joinWith '/' ("path" :| ["", "to", "", "file"]) `shouldBe`
        "path//to//file"

    it "join '' with '/'" $ do
      joinWith '/' ("" :| []) `shouldBe` ""

    it "join 'hello' with '/'" $ do
      joinWith '/' ("hello" :| []) `shouldBe` "hello"

    it "join [[2, 3, 4], [3, 10], [5]] with 1" $ do
      joinWith 1 ([2, 3, 4] :| [[3, 10], [5]]) `shouldBe`
        [2, 3, 4, 1, 3, 10, 1, 5]

  describe "join-split-id" $ do
    it "id 'hello' == 'hello'" $ do
      joinSplitId ('h' :| "ello") `shouldBe` "hello"

    it "id [1, 2, 3] == [1, 2, 3]" $ do
      joinSplitId (1 :| [2, 3]) `shouldBe` [1, 2, 3]

    it "id '' == ''" $ do
      joinWith '/' (splitOn '/' "") `shouldBe` ""