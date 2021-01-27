module Task5Spec
  ( spec
  ) where

import Task4.HalyavaScriptSamples
import Task5.HS2JS

import Test.Hspec

log2Res :: String
log2Res = unlines
  [ "function(v0) {"
  , "  var v1 = NaN;"
  , "  var v2 = 0.0;"
  , "  v2 = 1.0;"
  , "  v1 = 0.0;"
  , "  while ((v0) > (v2)) {"
  , "    v2 = (v2) + (v2);"
  , "    v1 = (v1) + (1.0);"
  , "  }"
  , "  return v1;"
  , "}"
  ]

averageSqrOnSegmentRes :: String
averageSqrOnSegmentRes = unlines
  [ "function(v0, v1) {"
  , "  var v2 = NaN;"
  , "  var v3 = 0.0;"
  , "  var v4 = 0.0;"
  , "  v4 = v0;"
  , "  while ((v4) <= (v1)) {"
  , "    v3 = (v3) + ((v4) * (v4));"
  , "    v4 = (v4) + (1.0);"
  , "  }"
  , "  v2 = (v3) / ((v1) - (v0));"
  , "  return v2;"
  , "}"
  ]

absSubRes :: String
absSubRes = unlines
  [ "function(v0, v1) {"
  , "  var v2 = NaN;"
  , "  if ((v0) >= (v1)) {"
  , "    v2 = (v0) - (v1);"
  , "  }"
  , "  else {"
  , "    v2 = (v1) - (v0);"
  , "  }"
  , "  return v2;"
  , "}"
  ]

spec :: Spec
spec = do
  it "log2" $ do
    convert1 log2 `shouldBe` log2Res

  it "averageSqrOnSegment" $ do
    convert2 averageSqrOnSegment `shouldBe` averageSqrOnSegmentRes

  it "absSub" $ do
    convert2 absSub `shouldBe` absSubRes