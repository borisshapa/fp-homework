{-# LANGUAGE ScopedTypeVariables    #-}

module Task4.HalyavaScriptSamples
  ( -- * HalyavaScript code examples
    absSub
  , averageSqrOnSegment
  , log2
  ) where

import Task4.HalyavaScript

-- | Binary logarithm rounding up.
log2 :: HalyavaScript hs => hs Val -> hs Val
log2 =
  sFun1 $ \a logCnt ->
  sWithVar (0 :: Double) $ \accum ->
    accum @= number (1 :: Double) #
    logCnt @= number (0 :: Double) #
    sWhile (a @> eRead accum)
      ( accum @= eRead accum + eRead accum #
        logCnt @= eRead logCnt + number (1 :: Double)
      )

-- | Mean value of squares on a segment
averageSqrOnSegment :: HalyavaScript hs => hs Val -> hs Val -> hs Val
averageSqrOnSegment =
  sFun2 $ \a b res ->
    sWithVar (0 :: Double) $ \accum ->
      sWithVar (0 :: Double) $ \iterator ->
        iterator @= eRead a #
        sWhile (iterator @<= eRead b)
          ( accum @= eRead accum + (eRead iterator * eRead iterator) #
            iterator @= eRead iterator + number (1 :: Double)
          ) #
      res @= accum @/ (b @- eRead a)

-- | Difference modulus
absSub :: HalyavaScript hs => hs Val -> hs Val -> hs Val
absSub =
  sFun2 $ \a b res ->
    sIf (a @>= eRead b)
      (res @= a @- eRead b)
      (res @= b @- eRead a)