import Data.List.Split (splitOn)
import Data.List (sort)

type Point = (Int, Int)
type Segment = (Point, Point)

intersectionToDistance :: Point -> Int
intersectionToDistance (x, y) = abs $ x + y

actionToDelta :: String -> (Int, Int)
actionToDelta (ch:num)
  | ch == 'R' = (read num, 0)
  | ch == 'L' = (-(read num), 0)
  | ch == 'U' = (0, read num)
  | ch == 'D' = (0, -(read num))
  | otherwise = error "Unexpected direction"
actionToDelta _ = error "Unexpected action format"

accumSegments :: ([Segment], Point) -> String -> ([Segment], Point)
accumSegments (segs, pos) action =
  let (x, y) = pos
      (dx, dy) = actionToDelta action
      newPos = (x + dx, y + dy)
  in
    (segs ++ [(pos, newPos)], newPos)

normalizeSegment :: Segment -> Segment
normalizeSegment ((x1, y1), (x2, y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

pathToSegments :: [String] -> [Segment]
pathToSegments actions =
  let (segments, _) = foldl accumSegments ([], (0, 0)) actions in map normalizeSegment $ segments

intersect4 :: (Int, Int, Int, Int) -> (Int -> Point) -> Maybe Point
intersect4 (a1, a2, b1, b2) f
  | a1 <= b1 && b1 <= a2 = Just (f b1)
  | b1 <= a1 && a1 <= b2 = Just (f a1)
  | otherwise = Nothing

findIntersection :: Segment -> Segment -> Maybe Point
findIntersection ((x1, y1), (x2, y2)) ((w1, z1), (w2, z2))
  -- two vertical lines
  | x1 == x2 && w1 == w2 = if x1 == w1 then intersect4 (y1, y2, z1, z2) (\y -> (x1, y)) else Nothing
  -- two horizontal lines
  | y1 == y2 && z1 == z2 = if y1 == z1 then intersect4 (x1, x2, w1, w2) (\x -> (x, y1)) else Nothing
  -- horizontal and vertical
  | y1 == y2 && w1 == w2 = if z1 <= y1 && y1 <= z2 && x1 <= w1 && w1 <= x2 then Just (w1, y1) else Nothing
  -- vertical and horizontal
  | x1 == x2 && z1 == z2 = if w1 <= x1 && x1 <= w2 && y1 <= z1 && z1 <= y2 then Just (x1, z1) else Nothing
  -- else
  | otherwise = Nothing

process :: [[String]] -> Int
process (xs:ys:_) =
  let segxs = pathToSegments xs
      segys = pathToSegments ys
      maybeIntersections = [findIntersection x y | x <- segxs, y <- segys]
      intersections = [intersection | Just intersection <- maybeIntersections]
  in
    head $ tail $ sort $ map intersectionToDistance intersections

process _ = error "Must be a list of at least 2 wires"

main :: IO ()
main = do
  input <- getContents
  print $ process $ map (splitOn ",") $ lines input
