{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Task5.HS2JS
  ( -- * The @Task5.HS2JS@ functions
    convert
  , convert1
  , convert2
  )where

import Control.Monad.State
import Control.Monad.Writer

import Task4.HalyavaScript

type ToJS = WriterT String (State (Int, Int))

newtype HS2JS s a = HS2JS
  { toJS :: ToJS a
  } deriving (Functor, Applicative, Monad)

-- | Makes an indent
tellWithIndent :: String -> ToJS ()
tellWithIndent str = do
  tab <- gets fst
  when (tab > 0 || tab == 0 && str == "}") (tell "\n")
  tell $ (replicate tab ' ') ++ str

-- | Returns the free name for the variable.
getName :: ToJS String
getName = do
  cur <- gets snd
  modify ((+1) <$>)
  return $ "v" ++ show cur

-- | An expression that returns nothing.
statementWithoutResult :: ToJS a -> ToJS ()
statementWithoutResult = void . statement

-- | An expression in HalyavaScript.
statement :: ToJS a -> ToJS a
statement action = do
  tab <- gets fst
  modify (\(_, ind) -> (0, ind))
  res <- action
  modify (\(_, ind) -> (tab, ind))
  return res

-- | Scope that returns nothing.
scopeWithoutResult :: ToJS a -> ToJS ()
scopeWithoutResult = void . scope

-- | Function, if or while scope
scope :: ToJS a -> ToJS a
scope body = do
  tell " {"
  modify (\(tab, ind) -> (tab + 2, ind))
  res <- body
  modify (\(tab, ind) -> (tab - 2, ind))
  tellWithIndent "}"
  return res

-- | Converts operation on mutable variables to JavaScript
opToJS :: String -> String -> HS2JS s Val -> HS2JS s Val
opToJS op a b = HS2JS $ do
  statementWithoutResult $ do
    tell $ "(" ++ a ++ ") " ++ op ++ " ("
    void $ toJS b
    tell ")"
  return Nan

-- | Creates new variable. Returns its name.
newVar :: ToJS Val -> ToJS String
newVar val = do
    valName <- getName
    tellWithIndent $ "var " ++ valName ++ " = "
    statementWithoutResult val
    tell ";"
    return valName

-- | Creates new variable and initialize it to NaN
emptyVar :: String -> ToJS ()
emptyVar varName = do
  tellWithIndent $ "var " ++ varName ++ " = "
  statementWithoutResult $ toJS nan
  tell ";"

-- | HalyavaScrit to JavaScript interpreter
instance HalyavaScript (HS2JS s) where
  type Var (HS2JS s) a = String

  number :: Double -> HS2JS s Val
  number a = HS2JS $ tell (show a) >> return Nan

  bool :: Bool -> HS2JS s Val
  bool True = HS2JS $ tell "true" >> return Nan
  bool False = HS2JS $ tell "false" >> return Nan

  nan :: HS2JS s Val
  nan = HS2JS $ tell "NaN" >> return Nan

  (@+) :: String -> HS2JS s Val -> HS2JS s Val
  (@+) = opToJS "+"

  (@-) :: String -> HS2JS s Val -> HS2JS s Val
  (@-) = opToJS "-"

  (@*) :: String -> HS2JS s Val -> HS2JS s Val
  (@*) = opToJS "*"

  (@/) :: String -> HS2JS s Val -> HS2JS s Val
  (@/) = opToJS "/"

  (@==) :: String -> HS2JS s Val -> HS2JS s Val
  (@==) = opToJS "=="

  (@!=) :: String -> HS2JS s Val -> HS2JS s Val
  (@!=) = opToJS "!="

  (@>) :: String -> HS2JS s Val -> HS2JS s Val
  (@>) = opToJS ">"

  (@<) :: String -> HS2JS s Val -> HS2JS s Val
  (@<) = opToJS "<"

  (@>=) :: String -> HS2JS s Val -> HS2JS s Val
  (@>=) = opToJS ">="

  (@<=) :: String -> HS2JS s Val -> HS2JS s Val
  (@<=) = opToJS "<="

  (#) :: HS2JS s a -> HS2JS s b -> HS2JS s b
  a # b = HS2JS $ do
    _ <- toJS a
    res <- toJS b
    return res

  (@=) :: String -> HS2JS s Val -> HS2JS s Val
  a @= b = HS2JS $ do
    tellWithIndent $ a ++ " = "
    statementWithoutResult $ toJS b
    tell ";"
    return Nan

  sNewVar :: HS2JS s Val -> HS2JS s String
  sNewVar = HS2JS . newVar . toJS

  sWithVar :: (ToVal a) => a -> (String -> HS2JS s b) -> HS2JS s b
  sWithVar x fun = HS2JS $ do
      var <- newVar $ toJS $ (valToHS $ toVal x)
      res <- toJS $ fun var
      return res

  sFun1 :: (String -> String -> HS2JS s a) -> HS2JS s Val -> HS2JS s Val
  sFun1 fun _ = HS2JS $ do
    argName <- getName
    retName <- getName
    tell $ "function(" ++ argName ++ ")"
    scopeWithoutResult $ do
      emptyVar retName
      void $ toJS $ fun argName retName
      tellWithIndent $ "return " ++ retName ++ ";"
      statement $ toJS $ return retName
    return Nan

  sFun2
    :: (String -> String -> String -> HS2JS s c)
    -> HS2JS s Val -> HS2JS s Val -> HS2JS s Val
  sFun2 fun _ _ = HS2JS $ do
      arg1Name <- getName
      arg2Name <- getName
      retName <- getName
      tell $ "function(" ++ arg1Name ++ ", " ++ arg2Name ++ ")"
      scopeWithoutResult $ do
        emptyVar retName
        void $ toJS $ fun arg1Name arg2Name retName
        tellWithIndent $ "return " ++ retName ++ ";"
        statement $ toJS $ return retName
      return Nan

  sIf :: HS2JS s Val -> HS2JS s a -> HS2JS s a -> HS2JS s a
  sIf condExpr a b = HS2JS $ do
      tellWithIndent "if ("
      statementWithoutResult $ toJS condExpr
      tell ")"
      res <- scope $ toJS a
      tellWithIndent "else"
      scopeWithoutResult $ toJS b
      return res

  sWhile :: HS2JS s Val -> HS2JS s a -> HS2JS s ()
  sWhile condExpr body = HS2JS $ do
      tellWithIndent "while ("
      statementWithoutResult $ toJS condExpr
      tell ")"
      scopeWithoutResult $ toJS body

  eRead :: Var (HS2JS s) Val -> HS2JS s Val
  eRead a = HS2JS $ tellWithIndent a >> return Nan

-- | Converts operations on immutable variables to JavaScript
toJSOp :: String -> HS2JS s Val -> HS2JS s Val -> HS2JS s Val
toJSOp op a b = HS2JS $ do
  tell "("
  void $ statement $ toJS a
  tell $ ") " ++ op ++ " ("
  void $ statement $ toJS b
  tell ")"
  return Nan

-- | Converts unary operations on immutable variables to JavaScript
unaryJSOp :: String -> HS2JS s Val -> HS2JS s Val
unaryJSOp op a = HS2JS $ do
  tell $ " " ++ op ++ " ("
  void $ statement $ toJS a
  tell ")"
  return Nan

instance Num (HS2JS s Val) where
    (+) = toJSOp "+"
    (-) = toJSOp "-"
    (*) = toJSOp "*"
    negate = unaryJSOp "-"
    abs = liftM abs
    signum = liftM signum
    fromInteger = number . fromInteger

-- | Converts function without arguments to JavaScript
convert :: HS2JS s a -> String
convert hs = (evalState (execWriterT $ toJS hs) (0, 0)) ++ "\n"

-- | Converts function with 1 argument to JavaScript
convert1 :: (HS2JS s Val -> HS2JS s a) -> String
convert1 hs = convert (hs nan)

-- | Converts function with 2 arguments to JavaScript
convert2 :: (HS2JS s Val -> HS2JS s Val -> HS2JS s a) -> String
convert2 hs = convert (hs nan nan)