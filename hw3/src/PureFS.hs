{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PureFS
  ( -- * PureFS types
    Directory(..)
  , File(..)
  , PureFS
  , SystemState(..)

    -- * PureFS functions
  , handlePath
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.List.Split
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T

import FSActions(FSActions(..), FSException(..), handlePath)

-- | Pure filesystem type
type PureFS = ExceptT FSException (State SystemState)

-- | Filesystem directory type
data Directory = Directory {
  dName :: String,      -- ^ directory name
  dFiles :: [File],     -- ^ files that directory contains
  dDirs :: [Directory]  -- ^ directories that directory contains
}

instance Show Directory where
  show dir = (dName dir) ++ "\n"
    ++ foldr (++) "" (map show (dFiles dir)) ++ "\n"
    ++ foldr (++) "" (map show (dDirs dir)) ++ "\n"


data File = File {
  fName :: String,     -- ^ file name
  fContent :: T.Text,  -- ^ file content
  fSize :: Integer     -- ^ file size
} deriving (Show)

-- Current filesystem state
data SystemState = SystemState {
  ssFileSystem :: Directory,    -- ^ filesystem tree
  ssWorkingDirectory :: String  -- ^ working directory
}

instance Show SystemState where
  show systemState = (ssWorkingDirectory systemState) ++ "\n"
    ++ (show $ ssFileSystem systemState)

instance Eq SystemState where
  (==) systemState1 systemState2 = (show systemState1) == (show systemState2)

-- | Returns files with passed name from passed directory
filterFilesEquals :: Directory -> String -> [File]
filterFilesEquals dir fileName =
  filter (\file -> (fName file) == fileName) (dFiles dir)

-- | Returns files with name that not equal passed one from passed directory
filterFilesNotEquals :: Directory -> String -> [File]
filterFilesNotEquals dir fileName =
  filter (\file -> (fName file) /= fileName) (dFiles dir)

-- | Returns directories with passed name from passed directory
filterDirsEquals :: Directory -> String -> [Directory]
filterDirsEquals dir dirName =
  filter (\folder -> (dName folder) == dirName) (dDirs dir)

-- | Returns directories with name
-- | that not equal passed one from passed directory
filterDirsNotEquals :: Directory -> String -> [Directory]
filterDirsNotEquals dir dirName =
  filter (\folder -> (dName folder) /= dirName) (dDirs dir)

-- | Removes dots from file path
removeDot :: [String] -> [String]
removeDot (".":xs) = removeDot xs
removeDot (x:xs) = x : (removeDot xs)
removeDot [] = []

-- | Splits a path into a list of directories
getDirsList :: FilePath -> [String]
getDirsList path = removeDot $ tail $ splitOn "/" path

-- | Traverses the filesystem tree through a list of directories
walkFileSystem :: Directory -> [String] -> Maybe Directory
walkFileSystem curDir [] = Just $ curDir
walkFileSystem curDir (headDir:dirs) = do
  let filteredDirs = filterDirsEquals curDir headDir
  if (length filteredDirs /= 0)
    then walkFileSystem (head filteredDirs) dirs
    else Nothing

-- | Create new directory by passed rule
createDir :: Directory
  -> [String]
  -> (Directory -> [String] -> a -> Directory)
  -> a
  -> Directory
createDir curDir (headDir:dirs) fun newFile =
  curDir {dDirs = changedDir : unchangedDirs}
  where
    toChange = head $ filterDirsEquals curDir headDir
    changedDir = fun toChange dirs newFile
    unchangedDirs = filterDirsNotEquals curDir headDir
createDir _ [] _ _ = Directory "" [] []

-- | Creates new directory in passed location
addDir :: Directory -> [String] -> Directory -> Directory
addDir curDir@Directory{dDirs=dirs} [] newDir =
  curDir{dDirs = newDir : dirs}
addDir curDir dirsList newDir =
  createDir curDir dirsList addDir newDir

-- | Creates new file in passed location
addFile :: Directory -> [String] -> File -> Directory
addFile curDir@Directory{dFiles = files} [] newFile =
  curDir{dFiles = newFile : files}
addFile curDir dirsList newFile =
  createDir curDir dirsList addFile newFile

-- | Removes directory from passed location
removeDir :: Directory -> [String] -> String -> Directory
removeDir curDir [] dirToDelete =
  curDir{dDirs = filterDirsNotEquals curDir dirToDelete}
removeDir curDir dirsList dirToDelete =
  createDir curDir dirsList removeDir dirToDelete

-- | Removes file from passed location
removeFile :: Directory -> [String] -> String -> Directory
removeFile curDir [] fileToDelete =
  curDir{dFiles = filterFilesNotEquals curDir fileToDelete}
removeFile curDir dirsList fileToDelete =
  createDir curDir dirsList removeFile fileToDelete

-- | Returns filepaths where files with passed name contain
findFileRecursive :: FilePath -> String -> Directory -> [FilePath]
findFileRecursive filePath fileName dir = do
  let files = filterFilesEquals dir fileName
  let res = map ((++) (filePath ++ "/")) $ map fName files
  let dirsRes = map (\childDir -> findFileRecursive
                      (filePath ++ "/" ++ (dName childDir))
                      fileName childDir) (dDirs dir)
  foldr (++) res dirsRes

instance FSActions PureFS where
  fileExists :: FilePath -> PureFS Bool
  fileExists filePath = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let absolutePath = handlePath curDirName filePath
    let absolutePathList = getDirsList absolutePath
    let dirPath = init absolutePathList
    let fileName = last absolutePathList
    let dir = walkFileSystem fileSystem dirPath
    return $ isJust dir
      && length (filterFilesEquals (fromJust dir) fileName) /= 0

  directoryExists :: FilePath -> PureFS Bool
  directoryExists dirPath = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let absolutePath = handlePath curDirName dirPath
    let absolutePathList = getDirsList absolutePath
    let dir = walkFileSystem fileSystem absolutePathList
    return $ isJust dir

  handleCd :: FilePath -> PureFS ()
  handleCd dirName = do
    curState@SystemState{ssWorkingDirectory = curDirName} <- get
    put curState{ssWorkingDirectory = handlePath curDirName dirName}

  handleLs :: FilePath -> PureFS String
  handleLs dirName = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = getDirsList $ handlePath curDirName dirName
    let dir = walkFileSystem fileSystem path
    let files = unlines $ (map fName (dFiles $ fromJust dir)) ++ (map dName (dDirs $ fromJust dir))
    return files

  createFolder :: String -> PureFS ()
  createFolder dirName = do
    curState@SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = getDirsList curDirName
    let newDir = Directory dirName [] []
    put curState{ssFileSystem = addDir fileSystem path newDir}

  handleCat :: FilePath -> PureFS String
  handleCat filePath = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = getDirsList $ handlePath curDirName filePath
    let dirPath = init path
    let dir = walkFileSystem fileSystem dirPath
    let fileName = last path
    let file = head $ filterFilesEquals (fromJust dir) fileName
    return $ T.unpack (fContent file)

  createFile :: String -> PureFS ()
  createFile filePath = do
    curState@SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = getDirsList $ handlePath curDirName filePath
    let dirPath = init path
    let fileName = last path
    let newFile = File fileName (T.pack "") 0
    put curState{ssFileSystem = addFile fileSystem dirPath newFile}

  handleRemove :: FilePath -> PureFS ()
  handleRemove filePath = do
    curState@SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = handlePath curDirName filePath
    let pathList = getDirsList path
    let dirList = init pathList
    let toDelete = last pathList
    fileExistsBool <- fileExists path
    dirExistsBool <- directoryExists path
    if (fileExistsBool)
      then put curState{ssFileSystem = removeFile fileSystem dirList toDelete}
      else if (dirExistsBool)
        then put curState{ssFileSystem = removeDir fileSystem dirList toDelete}
        else throw (FSException $ "File " ++ path ++ " doesn't exist")

  handleWriteFile :: FilePath -> String -> PureFS ()
  handleWriteFile filePath content = do
    curState@SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = handlePath curDirName filePath
    let pathList = getDirsList path
    let dirList = init pathList
    let toDelete = last pathList
    let fSwithDeletedFile = removeFile fileSystem dirList toDelete
    let newFile = File toDelete (T.pack content) (toInteger $ length content)
    put curState{ssFileSystem = addFile fSwithDeletedFile dirList newFile}

  findFile :: String -> PureFS String
  findFile fileName = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = getDirsList curDirName
    let curDir = walkFileSystem fileSystem path
    return $ unlines (findFileRecursive curDirName fileName (fromJust curDir))

  getFileSz :: FilePath -> PureFS Integer
  getFileSz filePath = do
    SystemState{ssFileSystem = fileSystem,
      ssWorkingDirectory = curDirName} <- get
    let path = handlePath curDirName filePath
    let pathList = getDirsList path
    let dirList = init pathList
    let fileName = last pathList
    let dir = walkFileSystem fileSystem dirList
    return $ fSize (head $ filterFilesEquals (fromJust dir) fileName)