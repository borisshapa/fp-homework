{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task3.ConcurrentHashTable
  ( -- * The @Task3.ConcurrentHashTable@ types
    ConcurrentHashTable (..)

    -- * The @Task3.ConcurrentHashTable@ functions
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (when)
import Control.Monad.STM (STM, atomically)
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Vector ((!))
import qualified Data.Vector as V

-- | The key-value pair
type Entry k v = TVar (Maybe (k, v))

-- | Vector of key-value pair
type HashTable k v = V.Vector (Entry k v)

-- | Concurrent hash table
data ConcurrentHashTable k v = ConcurrentHashTable
  { chtSize :: TVar Int              -- ^ hash table size
  , chtData :: TVar (HashTable k v)  -- ^ vector of key-value pairs
  }

-- | Creates empty vector with passed size.
newHT :: Int -> STM (HashTable k v)
newHT size = V.replicateM size (newTVar Nothing)

-- | Creates a new empty hash table with capacity = 16.
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  size <- newTVar 0
  emptyData <- newHT 16
  htData <- newTVar emptyData
  return $ ConcurrentHashTable size htData

-- | Returns an entry by key from passed hash table.
getEntry :: forall k v. (Hashable k, Eq k) =>
              k -> HashTable k v -> STM (Entry k v)
getEntry key htData = do
  findCyclically (hash key) (V.length htData)
    where
      findCyclically :: Int -> Int -> STM (Entry k v)
      findCyclically keyHash size = do
        let ind = keyHash `mod` size
        let entryTVar = htData ! ind
        entry <- readTVar entryTVar
        case entry of Just (entryKey, _) ->
                        if (key == entryKey)
                          then return entryTVar
                          else findCyclically (ind + 1) size
                      Nothing -> return entryTVar

-- | Returns a value by key from ConcurrentHashTable.
getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable _ htDataTVar) =
  atomically $ do
    htData <- readTVar htDataTVar
    entryTVar <- getEntry key htData
    entry <- readTVar entryTVar
    return $ case entry of Just (_, val) -> Just val
                           Nothing -> Nothing

-- | Creates a new key-value pair in the hash table, if no such key existed,
-- replaces the value otherwise.
putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key val (ConcurrentHashTable sizeTVar htDataTVar) = atomically $ do
  size <- readTVar sizeTVar
  htData <- readTVar htDataTVar
  let capacity = length htData
  entryTVar <- getEntry key htData
  entry <- readTVar entryTVar
  when (isNothing entry) (writeTVar sizeTVar (size + 1))
  writeTVar entryTVar (Just (key, val))
  when (size >= 3 * capacity `div` 4) $ do
    newHashTable <- reserve (2 * capacity) htData
    writeTVar htDataTVar newHashTable
    where
      reserve :: (Hashable k, Eq k) =>
        Int -> HashTable k v -> STM (HashTable k v)
      reserve newCapacity htData = do
        newHtData <- newHT newCapacity
        V.forM_ htData $ \entryTVar -> do
          entry <- readTVar entryTVar
          when (isJust entry) $ do
              let (entryKey, _) = fromJust entry
              newEntryTVar <- getEntry entryKey newHtData
              writeTVar newEntryTVar entry
        return newHtData

-- | Returns size of passed hash table
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable sizeTVar _) =
  atomically $ readTVar sizeTVar