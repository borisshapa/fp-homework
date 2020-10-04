{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( -- * The @Block1.Task2@ types
    Nat(..)

    -- * The @Block1.Task2@ functions
  , intToNat
  , isEven
  , natToInt
  ) where

-- | Data type for natural numbers.
data Nat
  = Z      -- ^ means 0.
  | S Nat  -- ^ means adding one or ' operation in Peano's axiomatics.
  deriving (Show)

-- | Nat is Number with arithmetic functions.
instance Num Nat where
  -- | Addition of two natural numbers.
  (+) :: Nat -> Nat -> Nat
  a + Z = a
  Z + a = a
  a + S bMinus1 = S a + bMinus1

  -- | Multiplication of two natural numbers.
  (*) :: Nat -> Nat -> Nat
  a * b =
    case a of
      Z -> Z
      S Z -> b
      S aMinus1 -> (aMinus1 * b) + b

  -- | Subtraction of natural numbers.
  -- If the result is less than 0, returns 0.
  (-) :: Nat -> Nat -> Nat
  Z - _ = Z
  a - Z = a
  S aMinus1 - S bMinus1 = aMinus1 - bMinus1

  -- | absolute value of natural number == natural namber.
  abs :: Nat -> Nat
  abs = id

  -- | signum of natural number
  --
  --    * 0 for Z
  --
  --    * 1 in other cases
  signum :: Nat -> Nat
  signum Z = intToNat 0
  signum _ = intToNat 1

  -- | Getting Nat by Integer. Throws an error if Integer is less than 0.
  fromInteger :: Integer -> Nat
  fromInteger a
    | a < 0     = error "natural number expected"
    | a == 0    = Z
    | otherwise = S (fromInteger (a - 1))

-- | Each natural number can be associated with
-- a non-negative integer number.
instance Enum Nat where
  -- | Converts Int to Nat
  toEnum :: Int -> Nat
  toEnum = fromInteger . toInteger

  -- | Converts Nat to Int
  fromEnum :: Nat -> Int
  fromEnum Z = 0
  fromEnum (S a) = fromEnum a + 1

  -- | Adding one to a natural number
  succ :: Nat -> Nat
  succ a = S a

  -- | Subtracting one from a natural number.
  -- If the result is less than 0, returns 0.
  pred :: Nat -> Nat
  pred a = a - toEnum 1

-- | Converting integers to natural numbers
intToNat :: Int -> Nat
intToNat = toEnum

-- | Converting natural numbers to integers
natToInt :: Nat -> Int
natToInt = fromEnum

instance Eq Nat where
  -- | Checking natural numbers for equality.
  (==) :: Nat -> Nat -> Bool
  Z == Z = True
  S a == S b = a == b
  _ == _ = False

-- | Nat numbers can be compared as Ints.
instance Ord Nat where
  -- | Comparison of natural numbers by less or equal.
   (<=) :: Nat -> Nat -> Bool
   Z <= _ = True
   S a <= S b = a <= b
   _ <= _ = False

instance Real Nat where
    -- | Converts Nat to Rational
    toRational :: Nat -> Rational
    toRational = toRational . fromEnum

instance Integral Nat where
    -- | Converts Nat to Integer
    toInteger :: Nat -> Integer
    toInteger Z = 0
    toInteger (S a) = toInteger a + 1

    -- | In the case of naturals, the same as divMod
    quotRem :: Nat -> Nat -> (Nat, Nat)
    quotRem = divMod

    -- | Integer result and remainder when dividing natural numbers.
    divMod :: Nat -> Nat -> (Nat, Nat)
    divMod _ Z = error "division by zero"
    divMod a b =
      if a < b
      then (Z, a)
      else (succ d, m) where
      (d, m) = divMod (a - b) b

-- | Checking a natural number for parity.
isEven :: Nat -> Bool
isEven a = mod2 == Z where
  (_, mod2) = quotRem a (fromInteger 2)