module FSActions 
  ( -- * FSActions types
    FSActions(..)
  , FSException(..)

    -- * FSActions functions
  , cat
  , cd
  , dir
  , handlePath
  , information
  , ls
  , remove
  , FSActions.writeFile
  ) where

import Control.Exception
import System.FilePath.Posix (normalise)

class Monad m => FSActions m where
  -- | Check if file exists
  fileExists :: FilePath -> m Bool
  -- | Check if directory exists
  directoryExists :: FilePath -> m Bool
  -- | Process 'cd' command
  handleCd :: FilePath -> m ()
  -- | Process 'ls' command
  handleLs :: FilePath -> m String
  -- | Create new folder commmand
  createFolder :: String -> m ()
  -- | Process 'cat' command
  handleCat :: FilePath -> m String
  -- | Create new file command
  createFile :: String -> m ()
  -- | Process 'remove' command
  handleRemove :: FilePath -> m ()
  -- | Process 'write-file' command
  handleWriteFile :: FilePath -> String -> m ()
  -- | Find file by name recursively
  findFile :: String -> m String
  -- | Get file size
  getFileSz :: FilePath -> m Integer

-- | Exception that occurs during file system working
data FSException = FSException String
  deriving (Show)

instance  Exception FSException

-- | Exception when file is missing
fileDoesntExistException :: String -> FSException
fileDoesntExistException fileName = FSException $
  "File " ++ fileName ++ " doesn't exist"

-- | Exception when directory is missing
dirDoesntExistException :: String -> FSException
dirDoesntExistException dirName = FSException $
  "Directory " ++ dirName ++ " doesn't exist"

-- | Deletes '.', handle '..' in path and concatinate with working directory
handlePath :: FilePath -> FilePath -> FilePath
handlePath workingDir filePath = 
  if (head normalisedFilePath == '/')
    then normalisedFilePath
    else workingDir ++ "/" ++ normalisedFilePath
  where
    normalisedFilePath = normalise filePath

-- | Changes the working directory
cd :: (FSActions m) => FilePath -> m ()
cd dirName = do
  dirExistsBool <- directoryExists dirName
  if (dirExistsBool)
    then handleCd dirName
    else throw $ dirDoesntExistException dirName

-- | Shows passed directory content
ls :: (FSActions m) => FilePath -> m String
ls dirName = do
  dirExistsBool <- directoryExists dirName
  if (dirExistsBool)
    then handleLs dirName
    else throw $ dirDoesntExistException dirName

-- | Show passed file content
cat :: (FSActions m) => FilePath -> m String
cat fileName = do
  fileExistsBool <- fileExists fileName
  if (fileExistsBool)
    then handleCat fileName
    else throw $ fileDoesntExistException fileName

-- | Show the working directory content
dir :: (FSActions m) => m String
dir = handleLs "."

-- | Remove passed file or directory
remove :: (FSActions m) => FilePath -> m ()
remove fileName = do
  fileExistsBool <- fileExists fileName
  dirExistsBool <- directoryExists fileName
  if (fileExistsBool || dirExistsBool)
    then handleRemove fileName
    else throw $ fileDoesntExistException fileName

-- | Writes text to file
writeFile :: (FSActions m) => FilePath -> String -> m ()
writeFile fileName content = do
  fileExistsBool <- fileExists fileName
  if (fileExistsBool)
    then handleWriteFile fileName content
    else throw $ fileDoesntExistException fileName

-- | Returns "directory" if directory was passed, "file" if file was passed
getFileType :: (FSActions m) => FilePath -> m String
getFileType fileName = do
  fileExistsBool <- fileExists fileName
  dirExistsBool <- directoryExists fileName
  if (fileExistsBool)
    then return "file"
    else if (dirExistsBool)
      then return "directory"
      else throw $ fileDoesntExistException fileName

-- | Shows file type and its size
information :: (FSActions m) => FilePath -> m String
information fileName = do
  fileType <- getFileType fileName
  fileSize <- getFileSz fileName
  return $ "Type: " ++ fileType ++ "\nSize: " ++ (show fileSize) ++ "B\n"