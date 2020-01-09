import Data.List (nubBy, maximumBy, groupBy, sortBy, transpose)
import Data.Ord (comparing)

dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
dist (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

compareAngle :: (Int, Int) -> (Int, Int) -> Ordering
compareAngle (x1, y1) (x2, y2) =
  if x1 >= 0 && x2 < 0 then LT
  else if x1 < 0 && x2 >= 0 then GT
  else (y1*x2) `compare` (y2*x1)

process :: [String] -> Int
process asteroidMap =
  let height = length asteroidMap
      width = length $ head $ asteroidMap
      isAsteroid = (==) '#'
      coords = [(x, y) | y <- [0..height-1], x <- [0..width-1]]
      asteroids = [coord | (coord, c) <- zip coords (concat asteroidMap), isAsteroid c]
      otherAsteroidVectors asteroid = [dist asteroid other | other <- asteroids, other /= asteroid]
      sameAngle a b = (compareAngle a b) == EQ
      visibleAsteroids asteroid = nubBy sameAngle $ otherAsteroidVectors asteroid
      station = fst $ maximumBy (comparing snd) $ zip asteroids $ map (length . visibleAsteroids) asteroids
      asteroidsWithDistance = [(asteroid, dist asteroid station) | asteroid <- asteroids, asteroid /= station]
      compareAngle' (_, a) (_, b) = compareAngle a b
      sameAngle' a b = (compareAngle' a b) == EQ
      groupedAsteroids = groupBy sameAngle' $ sortBy compareAngle' asteroidsWithDistance
      destroyedOrder = concat $ transpose groupedAsteroids
      ansAsteroid = fst $ destroyedOrder !! (200 - 1)
  in
    (fst ansAsteroid) * 100 + (snd ansAsteroid)

main :: IO ()
main = interact $ show . process . lines
