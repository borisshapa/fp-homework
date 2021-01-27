{-# LANGUAGE BangPatterns #-}

module Task6.FSLenses 
  ( -- * The @Task6.FSLenses@ types
    FS (..)

    -- * The @Task6.FSLenses@ functions
  , contents
  , getDirectory'
  , name

    -- * Prisms
  , _Dir
  , _File
  ) where

import Lens.Micro (Lens', Traversal', lens)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (dropTrailingPathSeparator, takeFileName, (</>))

-- Filesystem model
data FS 
  = Dir   -- ^ Filesystem direcory
  { _name     :: FilePath  -- ^ directory name, not full path
  , _contents :: [FS]      -- ^ directory contents
  }
  | File  -- ^ Filesystem file
  { _name     :: FilePath  -- ^ file name, not full path
  } deriving (Show)

-- | Creates an FS object based on the real file system.
-- The root is the passed path.
getDirectory' :: FilePath -> IO FS
getDirectory' path = do
  let objName = takeFileName $ dropTrailingPathSeparator path
  !isDirectory <- doesDirectoryExist path
  if (isDirectory)
    then do
      !objs <- listDirectory path
      !dContents <- mapM (getDirectory' . (path </>)) objs
      return $ Dir objName dContents
    else
      return $ File objName

-- | Lens for name
name :: Lens' FS FilePath
name = lens _name (\fs v -> fs { _name = v })

-- | Traversal for contents
contents :: Traversal' FS [FS]
contents f (Dir dName dContents) = (Dir dName) <$> f dContents
contents _ file@File{} = pure file

-- | Dir prism
_Dir :: Traversal' FS FS
_Dir f dir@Dir{} = f dir
_Dir _ fs = pure fs

-- | File prism
_File :: Traversal' FS FS
_File f file@File{} = f file
_File _ fs = pure fs