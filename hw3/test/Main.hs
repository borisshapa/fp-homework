module Main
  (
    main
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as T

import FSActions
  (
    cat
  , cd
  , createFile
  , createFolder
  , dir
  , findFile
  , information
  , ls
  , remove
  , writeFile
  )
import PureFS
  (
    Directory(..)
  , File(..)
  , PureFS
  , SystemState(..)
  )

import Test.Hspec

getFS :: PureFS () -> SystemState -> SystemState
getFS st fs = snd $ (runState $ runExceptT st) fs

main :: IO ()
main = hspec $ do
  let startFS = SystemState (Directory "" [] []) ""
  describe "commands" $ do
    let state1 = (createFolder "dir1") :: PureFS ()
    let fs = getFS state1 startFS
    it "create-folder" $ do
      fs `shouldBe` SystemState (Directory "" [] [Directory "dir1" [] []]) ""
    let state2 = (cd "dir1") :: PureFS ()
    let fs2 = getFS state2 fs
    it "cd" $ do
      fs2 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [] []]) "/dir1"
    let state3 = (createFile "newFile.hs") :: PureFS ()
    let fs3 = getFS state3 fs2
    it "create-file" $ do
      fs3 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "") 0
                        ] []]) "/dir1"
    let state4 = (FSActions.writeFile "newFile.hs" "I love haskell") :: PureFS ()
    let fs4 = getFS state4 fs3
    it "write-file" $ do
      fs4 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "I love haskell") 14
                        ] []]) "/dir1"
    let state5 = (cat "newFile.hs") :: PureFS String
    let stateAndOutput = (runState $ runExceptT state5) fs4
    let fs5 = snd stateAndOutput
    it "cat" $ do
      let (Right content) = fst stateAndOutput
      content `shouldBe` "I love haskell"
    let state6 = (createFolder "dir2") :: PureFS ()
    let fs6 = getFS state6 fs5
    it "create-folder 2" $ do
      fs6 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "I love haskell") 14
                        ] [
                          Directory "dir2" [] []
                        ]]) "/dir1"
    let state7 = (createFile "dir2/newFile.hs") :: PureFS ()
    let fs7 = getFS state7 fs6
    it "create-file 2" $ do
      fs7 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "I love haskell") 14
                        ] [
                          Directory "dir2" [
                            File "newFile.hs" (T.pack "") 0
                          ] []
                        ]]) "/dir1"
    let state8 = (findFile "newFile.hs") :: PureFS String
    let stateAndOutput2 = (runState $ runExceptT state8) fs7
    let fs8 = snd stateAndOutput2
    it "find-file" $ do
      let (Right content) = fst stateAndOutput2
      content `shouldBe` "/dir1/dir2/newFile.hs\n"
                        ++ "/dir1/newFile.hs\n"
    let state9 = (ls "dir2") :: PureFS String
    let stateAndOutput3 = (runState $ runExceptT state9) fs8
    let fs9 = snd stateAndOutput3
    it "ls" $ do
      let (Right content) = fst stateAndOutput3
      content `shouldBe` "newFile.hs\n"
    let state10 = (dir) :: PureFS String
    let stateAndOutput4 = (runState $ runExceptT state10) fs9
    let fs10 = snd stateAndOutput4
    it "dir" $ do
      let (Right content) = fst stateAndOutput4
      content `shouldBe` "newFile.hs\n"
                          ++ "dir2\n"
    let state11 = (remove "dir2/newFile.hs") :: PureFS ()
    let fs11 = getFS state11 fs10
    it "remove-file" $ do
      fs11 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "I love haskell") 14
                        ] [
                          Directory "dir2" [] []
                        ]]) "/dir1"
    let state12 = (remove "dir2") :: PureFS ()
    let fs12 = getFS state12 fs11
    it "remove-dir" $ do
      fs12 `shouldBe` SystemState
                      (Directory "" [] [Directory "dir1" [
                          File "newFile.hs" (T.pack "I love haskell") 14
                        ] []]) "/dir1"
    let state13 = (information "newFile.hs") :: PureFS String
    let stateAndOutput5 = (runState $ runExceptT state13) fs12
    it "information" $ do
      let (Right content) = fst stateAndOutput5
      content `shouldBe` "Type: file\n"
                        ++ "Size: 14B\n"