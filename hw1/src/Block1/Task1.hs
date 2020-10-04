{-# LANGUAGE InstanceSigs #-}

module Block1.Task1
  ( -- * The @Block1.Task1@ types
    DayOfWeek(..)

    -- * The @Block1.Task1@ functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- | The type contains constructors without parameters
-- that are responsible for the days of the week.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

-- | The same days of the week are equal.
instance Eq DayOfWeek where
  (==) :: DayOfWeek -> DayOfWeek -> Bool
  Monday == Monday       = True
  Tuesday == Tuesday     = True
  Wednesday == Wednesday = True
  Thursday == Thursday   = True
  Friday == Friday       = True
  Saturday == Saturday   = True
  Sunday == Sunday       = True
  _ == _                 = False

-- | Returns an element from an array.
-- It does not throw an error when the array is out of bounds,
-- but returns the element with the index equal 'ind' % 'length of array'.
elementFromCyclicalList :: [a] -> Int -> a
elementFromCyclicalList list index = list !! cyclicalIndex
  where
    cyclicalIndex = index `mod` length list

-- | A function that returns the order of days in a week.
daysOfWeekOrder :: [DayOfWeek]
daysOfWeekOrder =
  [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

-- | Returns the day of the week by number 'ind' % 7
dayOfWeekByNumber :: Int -> DayOfWeek
dayOfWeekByNumber = elementFromCyclicalList daysOfWeekOrder

-- | Each day is associated with its serial number in the week.
instance Enum DayOfWeek where
  -- | day number -> day of week
  toEnum :: Int -> DayOfWeek
  toEnum = dayOfWeekByNumber

  -- | day of week -> day number
  fromEnum :: DayOfWeek -> Int
  fromEnum day = fromJust (elemIndex day daysOfWeekOrder)

-- | Returns the next day of the week after the passed one.
nextDay :: DayOfWeek -> DayOfWeek
nextDay dayOfWeek = succ dayOfWeek

-- | Returns the day of the week after the specified number
-- of days after the passed.
afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays dayOfWeek daysCount = toEnum (fromEnum dayOfWeek + daysCount)

-- | Checks if the day of the week is a holiday.
isWeekend :: DayOfWeek -> Bool
isWeekend dayOfWeek = fromEnum dayOfWeek > 4  -- 4 because we use 0-based indexing

-- | Returns the number of days remaining until Friday.
daysToParty :: DayOfWeek -> Int
daysToParty dayOfWeek = (difference + 7) `mod` 7
  where
    difference = fromEnum Friday - fromEnum dayOfWeek