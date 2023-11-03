module AocPoint (Point, PointMap (..), getPoint, getRow, getCol, getCardinalIndicies, getOrdinalIndicies, getCardinalPoints, getOrdinalPoints) where

type Point = (Int, Int)

-- | A flat map of 2d points
data PointMap = PointMap {size :: (Int, Int), points :: [Point]} deriving (Show)

-- | get a point from a point map will crash if index is out of bounds
getPoint :: PointMap -> Point -> Point
getPoint (PointMap (w, _) ps) (x, y) = ps !! (w * y + x)

-- | get a row of points from a point map will return empty on out of bounds
getRow :: PointMap -> Int -> [Point]
getRow (PointMap (w, _) ps) y = take w $ drop (w * y) ps

-- | get a column of points from a point map will crash if index is out of bounds
getCol :: PointMap -> Int -> [Point]
getCol (PointMap (w, l) ps) x = [p | i <- [1 .. (w * l)], (i `mod` w) == 0, p <- [ps !! (i - (w - x))]]

-- | get indices N,E,S,W of an index
getCardinalIndicies :: Point -> [Point]
getCardinalIndicies (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- | get all surrounding indcies of an index
getOrdinalIndicies :: Point -> [Point]
getOrdinalIndicies (x, y) =
  filter
    (/= (x, y)) -- don't include given index
    [ (a, b)
      | a <- [p + x | p <- [-1 .. 1]],
        b <- [i + y | i <- [-1 .. 1]]
    ]

-- | check if a point has either x, or y out of bounds of the point map's size
indexInRange :: PointMap -> Point -> Bool
indexInRange pm (x, y) = x >= 0 && x < fst (size pm) && y >= 0 && y < snd (size pm)

-- | get the points N,E,S,W of the given index
getCardinalPoints :: PointMap -> Point -> [Point]
getCardinalPoints pm p = map (getPoint pm) $ filter (indexInRange pm) $ getCardinalIndicies p

-- | get all the points around the given index
getOrdinalPoints :: PointMap -> Point -> [Point]
getOrdinalPoints pm p = map (getPoint pm) $ filter (indexInRange pm) $ getOrdinalIndicies p
