{-# LANGUAGE RankNTypes #-}

module Main
    (
      main
    ) where

import Data.IORef
import Control.Monad()
import Control.Monad.Reader
import Options.Applicative.Extra
import System.Environment (getArgs)

import Parser (Command(..), parse)
import RealFS()
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
  )

-- | How to process the command in CLI.
handleCommand :: Command -> IORef FilePath -> IO ()
handleCommand (Cd dirName) env = runReaderT (cd dirName) env

handleCommand (Ls dirName) env = do
  text <- runReaderT (ls dirName) env
  putStrLn text

handleCommand (CreateFolder dirName) env =
  runReaderT (createFolder dirName) env

handleCommand (Cat fileName) env = do
  text <- runReaderT (cat fileName) env
  putStrLn text

handleCommand (CreateFile fileName) env = do
  runReaderT (createFile fileName) env

handleCommand (Remove fileName) env = do
  runReaderT (remove fileName) env

handleCommand (FindFile fileName) env = do
  text <- runReaderT (findFile fileName) env
  putStrLn text

handleCommand (Information fileName) env = do
  text <- runReaderT (information fileName) env
  putStrLn text

handleCommand Dir env = do 
  text <- runReaderT dir env
  putStrLn text

handleCommand _ _ =
  putStrLn $ "cd <folder> -- перейти в директорию\n"
            ++ "dir -- показать содержимое текущей директории\n"
            ++ "ls <folder> -- показать содержимое выбранной директории\n"
            ++ "create-folder \"folder-name\" -- создать директорию в текущей\n"
            ++ "cat <file> -- показать содержимое файла\n"
            ++ "create-file \"file-name\" "
              ++ "-- создать пустой файл в текущей директории\n"
            ++ "remove <folder | file> "
              ++ "-- удалить выборанную директорию или файл\n"
            ++ "write-file <file> \"text\" -- записать текст в файл\n"
            ++ "find-file \"file-name\" "
              ++ "--  поиск файла в текущией директории и поддиректориях\n"
            ++ "information <file> -- показать информацию о файле\n"
            ++ "information <folder> -- показать информацию о директории\n"
            ++ "help --  показать руководство по использованию\n"

main :: IO ()
main = do
  args <- getArgs
  let workingDirectory = head args
  ref <- newIORef workingDirectory
  forever $ do
    command <- getLine
    let parsingResult = parse $ words command
    case parsingResult of
      Success cmd -> handleCommand cmd ref
      Failure _ -> putStrLn "Fail"
      CompletionInvoked _ -> putStrLn "compinv"