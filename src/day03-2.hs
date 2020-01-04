import Data.List.Split (splitOn)
import Data.List (sort)

type Point = (Int, Int)
type Segment = (Point, Point, Int)

actionToDelta :: String -> (Point, Int)
actionToDelta (ch:num)
  | ch == 'R' = ((read num, 0), read num)
  | ch == 'L' = ((-(read num), 0), read num)
  | ch == 'U' = ((0, read num), read num)
  | ch == 'D' = ((0, -(read num)), read num)
  | otherwise = error "Unexpected direction"
actionToDelta _ = error "Unexpected action format"

accumSegments :: ([Segment], Point, Int) -> String -> ([Segment], Point, Int)
accumSegments (segs, pos, c) action =
  let (x, y) = pos
      ((dx, dy), dc) = actionToDelta action
      newPos = (x + dx, y + dy)
      newC = c + dc
  in
    (segs ++ [(pos, newPos, c)], newPos, newC)

pathToSegments :: [String] -> [Segment]
pathToSegments actions =
  let (segments, _, _) = foldl accumSegments ([], (0, 0), 0) actions in segments

dist :: Int -> Int -> Int
dist x1 x2 = abs (x1 - x2)

inSegment :: Int -> Int -> Int -> Bool
inSegment a b1 b2 = (min b1 b2) <= a && a <= (max b1 b2)

intersect4 :: (Int, Int, Int, Int) -> Int -> [Int]
intersect4 (a1, a2, b1, b2) c =
  [(c + (dist x a1) + (dist x b1)) |
   -- generate all possible key points that can be intersections
    (x, o1, o2) <- [(a1, b1, b2), (a2, b1, b2), (b1, a1, a2), (b2, a1, a2)],
    -- ensure that is actually an intersection
    inSegment x o1 o2]

findIntersections :: Segment -> Segment -> [Int]
findIntersections ((x1, y1), (x2, y2), c1) ((w1, z1), (w2, z2), c2)
  -- two vertical lines
  | x1 == x2 && w1 == w2 = if x1 == w1 then intersect4 (y1, y2, z1, z2) (c1 + c2) else []
  -- two horizontal lines
  | y1 == y2 && z1 == z2 = if y1 == z1 then intersect4 (x1, x2, w1, w2) (c1 + c2) else []
  -- horizontal and vertical
  | y1 == y2 && w1 == w2 = if inSegment y1 z1 z2 && inSegment w1 x1 x2 then [c1 + c2 + (dist w1 x1) + (dist z1 y1)] else []
  -- vertical and horizontal
  | x1 == x2 && z1 == z2 = if inSegment x1 w1 w2 && inSegment z1 y1 y2 then [c1 + c2 + (dist w1 x1) + (dist z1 y1)] else []
  -- else
  | otherwise = []

process :: [[String]] -> Int
process (xs:ys:_) =
  let segxs = pathToSegments xs
      segys = pathToSegments ys
      intersections = concat [findIntersections x y | x <- segxs, y <- segys]
  in
    head $ sort $ filter (/=0) intersections

process _ = error "Must be a list of at least 2 wires"

main :: IO ()
main = do
  input <- getContents
  print $ process $ map (splitOn ",") $ lines input
