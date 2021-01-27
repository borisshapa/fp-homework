{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Task4.HalyavaScript
  ( -- * The @Task4.HalyavaScript@ types
    HalyavaScript (..)
  , ToVal (..)
  , Val (..)

    -- * The @Task4.HalyavaScript@ functions
  , run
  , run1
  , run2
  ) where

import Control.Applicative (liftA2)
import Control.Monad (liftM, liftM2, when)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | Available data types in HalyavaScript
data Val
  = Number Double
  | Boolean Bool
  | Nan
  deriving (Show, Eq)

instance Num Val where
  (+) :: Val -> Val -> Val
  (+) = op (+)

  (*) :: Val -> Val -> Val
  (*) = op (*)

  negate :: Val -> Val
  negate = unaryOp negate

  abs :: Val -> Val
  abs = unaryOp abs

  signum :: Val -> Val
  signum = unaryOp signum

  fromInteger :: Integer -> Val
  fromInteger x = Number $ fromInteger x

-- | Class for Haskell data types that can be converted to
-- HalyavaScript data types
class ToVal a where
  toVal :: a -> Val

instance ToVal Double where
  toVal = Number

instance  ToVal Bool where
  toVal = Boolean

-- | Availible operations in HalyavaScript
class (Monad expr, Num (expr Val)) => HalyavaScript expr where
  -- | Mutable variable type family.
  type Var expr a :: *

  -- | var + val
  infixl 6 @+
  (@+) :: Var expr Val -> expr Val -> expr Val

  -- | var - val
  infixl 6 @-
  (@-) :: Var expr Val -> expr Val -> expr Val

  -- | var * val
  infixl 7 @*
  (@*) :: Var expr Val -> expr Val -> expr Val

  -- | var / val
  infixl 7 @/
  (@/) :: Var expr Val -> expr Val -> expr Val

  -- | var == val
  infix 4 @==
  (@==) :: Var expr Val -> expr Val -> expr Val

  -- | var != val
  infix 4 @!=
  (@!=) :: Var expr Val -> expr Val -> expr Val

  -- | var < val
  infix 4 @<
  (@<) :: Var expr Val -> expr Val -> expr Val

  -- | var > val
  infix 4 @>
  (@>) :: Var expr Val -> expr Val -> expr Val

  -- | var <= val
  infix 4 @<=
  (@<=) :: Var expr Val -> expr Val -> expr Val

  -- | var >= val
  infix 4 @>=
  (@>=) :: Var expr Val -> expr Val -> expr Val

  -- | Separates computations
  infixl 0 #
  (#) :: expr a -> expr b -> expr b

  -- | Assignment operator
  infix 1 @=
  (@=) :: Var expr Val -> expr Val -> expr Val

  -- | Converts Double to Number in monad
  number :: Double -> expr Val
  number = return . toVal

  -- | Converts Bool to Boolean in Monad
  bool :: Bool -> expr Val
  bool = return . toVal

  -- | Nan in monad
  nan :: expr Val
  nan = return Nan

  -- | Immerses meaning in context
  valToHS :: Val -> expr Val
  valToHS (Number a) = number a
  valToHS (Boolean b) = bool b
  valToHS Nan = nan

  -- | Creates new mutable variable
  sNewVar :: expr Val -> expr (Var expr Val)

  -- | A calculation in which the passed value is assigned
  -- to the variable (the argument of the function-argument).
  sWithVar :: (ToVal a) => a -> (Var expr Val -> expr b) -> expr b
  sWithVar x fun = (sNewVar $ (valToHS $ toVal x)) >>= fun

  -- | Accepts a function where the second argument is the return value,
  -- as well as the value of the first argument. Returns the return value.
  sFun1 :: (Var expr Val -> Var expr Val -> expr a) -> expr Val -> expr Val

  -- | Accepts a function where the third argument is the return value,
  -- as well as the values of the first and the second arguments.
  -- Returns the return value.
  sFun2
    :: (Var expr Val -> Var expr Val -> Var expr Val -> expr c)
    -> expr Val -> expr Val -> expr Val

  -- | Conditional expression.
  sIf :: expr Val -> expr a -> expr a -> expr a

  -- | While loop
  sWhile :: expr Val -> expr a -> expr ()

  -- | Returns the value of mutable variable
  eRead :: Var expr Val -> expr Val

-- | Converts Bool to Double: True -> 1.0, False -> 0.0
toDouble :: Bool -> Double
toDouble b = fromIntegral $ fromEnum b

-- | Takes binary operations in Double and arguments in Val.
-- Applies the operation.
op :: (ToVal a) => (Double -> Double -> a) -> Val -> Val -> Val
op _ Nan _ = Nan
op _ _ Nan = Nan
op fun (Number a) (Number b) = toVal $ fun a b
op fun (Boolean a) (Boolean b) = toVal $ fun (toDouble a) (toDouble b)
op fun (Number a) (Boolean b) = toVal $ fun a (toDouble b)
op fun (Boolean a) (Number b) = toVal $ fun (toDouble a) b

-- | Takes unary operations in Double and arguments in Val.
-- Applies the operation.
unaryOp :: (ToVal a) => (Double -> a) -> Val -> Val
unaryOp _ Nan = Nan
unaryOp f (Number a) = toVal $ f a
unaryOp f (Boolean b) = toVal $ (f $ toDouble b)

-- | divide for Val
divide :: Val -> Val -> Val
divide (Number 0) (Number 0) = Nan
divide (Boolean False) (Boolean False) = Nan
divide a b = op (/) a b

-- | Analogue op, but returns Boolean.
binOp :: (ToVal a) => (Double -> Double -> a) -> Val -> Val -> Val
binOp _ Nan _ = Boolean False
binOp _ _ Nan = Boolean False
binOp fun a b = op fun a b

-- | Equals
eq :: Val -> Val -> Val
eq = binOp (==)

-- | Not equals
ne :: Val -> Val -> Val
ne = binOp (/=)

-- | Greater
gt :: Val -> Val -> Val
gt = binOp (>)

-- | Less
lt :: Val -> Val -> Val
lt = binOp (<)

-- | Greater or equal
ge :: Val -> Val -> Val
ge = binOp (>=)

-- | Less or equal
le :: Val -> Val -> Val
le = binOp (<=)

-- | Converts Val to Bool
toBool :: Val -> Bool
toBool Nan = False
toBool (Number 0) = False
toBool (Number _) = True
toBool (Boolean b) = b

-- | Raises operation in Val to ST monad level
refOp :: (Val -> Val -> Val) -> STRef s Val -> ST s Val -> ST s Val
refOp fun a b = (liftA2 fun) (readSTRef a) b

-- | HalyavaScrit to Haskell interpreter
instance HalyavaScript (ST s) where
  type Var (ST s) a = STRef s a

  sNewVar :: ST s Val -> ST s (STRef s Val)
  sNewVar var = var >>= newSTRef

  (@+) :: STRef s Val -> ST s Val -> ST s Val
  (@+) = refOp (+)

  (@-) :: STRef s Val -> ST s Val -> ST s Val
  (@-) = refOp (-)

  (@*) :: STRef s Val -> ST s Val -> ST s Val
  (@*) = refOp (*)

  (@/) :: STRef s Val -> ST s Val -> ST s Val
  (@/) = refOp divide

  (@==) :: STRef s Val -> ST s Val -> ST s Val
  (@==) = refOp eq

  (@!=) :: STRef s Val -> ST s Val -> ST s Val
  (@!=) = refOp ne

  (@>) :: STRef s Val -> ST s Val -> ST s Val
  (@>) = refOp gt

  (@<) :: STRef s Val -> ST s Val -> ST s Val
  (@<) = refOp lt

  (@>=) :: STRef s Val -> ST s Val -> ST s Val
  (@>=) = refOp ge

  (@<=) :: STRef s Val -> ST s Val -> ST s Val
  (@<=) = refOp le

  (#) :: ST s a -> ST s b -> ST s b
  (#) = (>>)

  (@=) :: STRef s Val -> ST s Val -> ST s Val
  a @= b = do
    val <- b
    writeSTRef a val
    return val

  sFun1 :: (STRef s Val -> STRef s Val -> ST s a) -> ST s Val -> ST s Val
  sFun1 fun argST = do
    var <- newSTRef Nan
    arg <- argST >>= newSTRef
    _ <- fun arg var
    readSTRef var

  sFun2
    :: (STRef s Val -> STRef s Val -> STRef s Val -> ST s c)
    -> ST s Val -> ST s Val -> ST s Val
  sFun2 fun arg1ST arg2ST = do
    arg1 <- arg1ST >>= newSTRef
    sFun1 (fun arg1) arg2ST

  sIf :: ST s Val -> ST s a -> ST s a -> ST s a
  sIf condExpr a b = do
    cond <- condExpr
    if (toBool cond)
      then a
      else b

  sWhile :: ST s Val -> ST s a -> ST s ()
  sWhile condExpr body = do
    cond <- condExpr
    when (toBool cond) $ body >> sWhile condExpr body

  eRead :: Var (ST s) Val -> ST s Val
  eRead = readSTRef

instance Num (ST s Val) where
    (+) = liftM2 (+)
    (-) = liftM2 (-)
    (*) = liftM2 (*)
    negate = liftM negate
    abs = liftM abs
    signum = liftM signum
    fromInteger = return . fromInteger

-- | Runs function without arguments
run :: (forall s. ST s a) -> a
run = runST

-- | Runs function with 1 argument
run1 :: (forall s. ST s Val -> ST s a) -> Val -> a
run1 hs arg = run $ hs (return arg)

-- | Runs function with 2 arguments
run2 :: (forall s. ST s Val -> ST s Val -> ST s a) -> Val -> Val -> a
run2 hs arg1 arg2 = run $ hs (return arg1) (return arg2)