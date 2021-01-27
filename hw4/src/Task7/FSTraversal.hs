{-# LANGUAGE Rank2Types #-}

module Task7.FSTraversal
  ( -- * The @Task7.FSTraversal@ functions
    cd
  , file
  , ls
  , test
  )where

import Lens.Micro (Traversal', filtered, (^.), (^..), (^?))

import Task6.FSLenses

-- | Traverses drectory contents
traverseContents :: Traversal' FS FS
traverseContents = contents . traverse

-- | Moves to passed directory
cd :: FilePath -> Traversal' FS FS
cd path = traverseContents . _Dir . filtered (\obj -> (obj ^. name) == path)

-- | Returns directory contents
ls :: Traversal' FS FilePath
ls = traverseContents . name

-- | Returns file name
file :: FilePath -> Traversal' FS FilePath
file path = traverseContents . _File . name . filtered (== path)

-- | function for test from ghci
test :: IO ()
test = do
  fs <- getDirectory' "."
  putStrLn $ show (fs ^? cd "src" . cd "Task1" . file "Lazy.hs")
  putStrLn $ unlines (fs ^.. cd "benchmark" . ls)