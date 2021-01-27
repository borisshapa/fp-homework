module Block2.Task1
  ( -- * The @Block1.Task3@ types
    ArithmeticError(..)
  , Expr(..)

    -- * The @Block1.Task3@ functions
  , eval
  ) where

-- | Arithmetic expression
data Expr
  = Const Int           -- Constant
  | Add Expr Expr       -- Addition operation
  | Subtract Expr Expr  -- Subtraction operation
  | Multiply Expr Expr  -- Multiplication operation
  | Divide Expr Expr    -- Division operation
  | Pow Expr Expr       -- Exponentiation operation
  deriving (Show)

-- | Arithmetic expression calculation error
data ArithmeticError
  = DivisionByZero  -- Division by zero
  | NegativeDegree  -- Negative exponentiation
  deriving (Show, Eq)

-- | The result of evaluating a mathematical expression or an error
type Result = Either ArithmeticError Int

-- | Operation calculation
binaryOperation :: (Int -> Int -> Result) -> Expr -> Expr -> Result
binaryOperation op l r = do
  lEval <- eval l
  rEval <- eval r
  op lEval rEval

-- | Calculating an operation without error
safeBinaryOperation :: (Int -> Int -> Int) -> Expr -> Expr -> Result
safeBinaryOperation op l r = binaryOperation ((return . ) . op) l r

-- | Evaluating an arithmetic expression
eval :: Expr -> Result
eval (Const a)      = Right a
eval (Add a b)      = safeBinaryOperation (+) a b
eval (Subtract a b) = safeBinaryOperation (-) a b
eval (Multiply a b) = safeBinaryOperation (*) a b
eval (Divide a b)      = binaryOperation (\numerator denominator ->
  if denominator == 0
  then Left DivisionByZero
  else return (numerator `div` denominator)) a b
eval (Pow a b)      = binaryOperation (\base power ->
  if power < 0
  then Left NegativeDegree
  else return (base ^ power)) a b