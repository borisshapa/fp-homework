{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task1.Strict
  ( -- * The @Task1.Strict@ types
    Point(..)

    -- * The @Task1.Strict@ functions
  , doubleArea
  , crossProduct
  , minus
  , perimeter
  , plus
  , scalarProduct
  ) where

-- | Point on a geometric plane with strict evaluation
data Point = Point
  { pX :: !Int -- ^ x-coordinate
  , pY :: !Int -- ^ y-coordivate
  } deriving (Show)

-- | The sum of the corresponding coordinates of two points.
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | The difference of the corresponding coordinates of two points.
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar product of radius vectors specified by points.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Vector product of radius vectors specified by points.
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Distance between points
distance :: Point -> Point -> Double
distance (Point ax ay) (Point bx by) =
  sqrt $ fromIntegral $ (bx - ax) ^ power + (by - ay) ^ power
    where
      power = (2 :: Integer)

-- | Summarizes the results of the edge operation.
-- Edges are defined by points.
polygonOperation :: forall a. Num a => (Point -> Point -> a) -> [Point] -> a
polygonOperation _ [] = 0
polygonOperation fun pointList@(first : _) = reduce 0 first pointList
  where
    reduce :: a -> Point -> [Point] -> a
    reduce _ _ [] = 0
    reduce !acc firstPoint [p] = acc + fun p firstPoint
    reduce !acc firstPoint (p1 : t@(p2 : _)) =
      reduce (acc + fun p1 p2) firstPoint t

-- | Perimeter of a figure.
perimeter :: [Point] -> Double
perimeter = polygonOperation distance

-- | Double area of a figure.
doubleArea :: [Point] -> Int
doubleArea = polygonOperation crossProduct