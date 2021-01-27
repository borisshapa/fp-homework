module Task1.Lazy
  ( -- * The @Task1.Lazy@ types
    LazyPoint(..)

    -- * The @Task1.Lazy@ functions
  , slowDoubleArea 
  , slowPerimeter
  ) where

-- | Point on a geometric plane with lazy evaluation
data LazyPoint = LazyPoint
  { lpX :: Int -- ^ x coordinate
  , lpY :: Int -- ^ y coordinate
  } deriving (Show)

-- | Edge on a geometric plane with lazy evaluation
data LazyEdge = LazyEdge
  { leA :: LazyPoint
  , leB :: LazyPoint
  } deriving (Show)

-- | Edge length
edgeLength :: LazyEdge -> Double
edgeLength (LazyEdge (LazyPoint ax ay) (LazyPoint bx by)) =
  sqrt $ fromIntegral $ (bx - ax) ^ power + (by - ay) ^ power
    where
      power = (2 :: Integer)

-- | Return edges that define a path defined by points.
edges :: [LazyPoint] -> [LazyEdge]
edges points = map (uncurry LazyEdge) $ zip points (tail $ cycle points)

-- | Double area of a trapezoid with bases parallel to the y-axis
-- and a given edge.
doubleTrapezoidArea :: LazyEdge -> Int
doubleTrapezoidArea (LazyEdge (LazyPoint ax ay) (LazyPoint bx by)) =
  (ay + by) * (ax - bx)

-- | Summarizes the results of the edge operation.
-- Edges are defined by points.
polygonOperation :: Num a => (LazyEdge -> a) -> [LazyPoint] -> a
polygonOperation fun pointList = sum $ map fun (edges pointList)

-- | Slow computation of the perimeter of a figure.
slowPerimeter :: [LazyPoint] -> Double
slowPerimeter = polygonOperation edgeLength

-- | Slow computation of the double area of a figure.
slowDoubleArea :: [LazyPoint] -> Int
slowDoubleArea = polygonOperation doubleTrapezoidArea