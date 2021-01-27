{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module RealFS
  (
    RealFS
  ) where

import Control.Monad.Reader
import Data.IORef
import System.Directory
  (
    createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findFiles
  , getFileSize
  , listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath.Posix (takeDirectory)
import System.IO (writeFile)

import FSActions (FSActions(..), handlePath)

-- | Type that works with real filesystem
type RealFS = ReaderT (IORef FilePath) IO

-- | Returns absolute file path
getAbsolutePath :: IORef FilePath -> FilePath -> IO FilePath
getAbsolutePath env filePath = (flip handlePath) filePath <$> readIORef env

instance FSActions RealFS where
  handleCd :: FilePath -> RealFS ()
  handleCd dirName = do
    env <- ask
    liftIO $ writeIORef env =<< (getAbsolutePath env dirName)

  fileExists :: FilePath -> RealFS Bool
  fileExists fileName = do
    env <- ask
    liftIO $ doesFileExist =<< getAbsolutePath env fileName

  directoryExists :: FilePath -> RealFS Bool
  directoryExists dirName = do
    env <- ask
    liftIO $ doesDirectoryExist =<< (getAbsolutePath env dirName)

  handleLs :: FilePath -> RealFS String
  handleLs dirName = do
    env <- ask
    liftIO $ unlines <$> (listDirectory =<< (getAbsolutePath env dirName))

  createFolder :: String -> RealFS ()
  createFolder dirName = liftIO $
    createDirectoryIfMissing True dirName

  handleCat :: FilePath -> RealFS String
  handleCat fileName = do
    env <- ask
    liftIO $ readFile =<< (getAbsolutePath env fileName)

  createFile :: String -> RealFS ()
  createFile fileName = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory fileName
    liftIO $ System.IO.writeFile fileName ""

  handleRemove :: FilePath -> RealFS ()
  handleRemove fileOrDirName = liftIO $ removeDirectoryRecursive fileOrDirName

  handleWriteFile :: FilePath -> String -> RealFS ()
  handleWriteFile fileName content =
    liftIO $ System.IO.writeFile fileName content

  findFile :: String -> RealFS String
  findFile fileName = do
    env <- ask
    let currentDirecotry = readIORef env
    let dirList = join $ walkDirsRecursive <$> currentDirecotry
    liftIO $ unlines <$> ((flip findFiles) fileName =<< dirList)
      where
        walkDirsRecursive :: FilePath -> IO [FilePath]
        walkDirsRecursive dirName = do
          children <- listDirectory dirName
          let canonicalizedChildren = map (handlePath dirName) children
          filteredChildren <- filterM doesDirectoryExist canonicalizedChildren
          dirList <- mapM walkDirsRecursive filteredChildren
          return $ foldr (++) [dirName] dirList

  getFileSz :: FilePath -> RealFS Integer
  getFileSz fileName = liftIO $ getFileSize fileName