import Data.List (nubBy)

dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
dist (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

sameAngle :: (Int, Int) -> (Int, Int) -> Bool
sameAngle (a1, b1) (a2, b2) = a1 * b2 == a2 * b1 && signum a1 == signum a2 && signum b1 == signum b2

process :: [String] -> Int
process asteroidMap =
  let height = length asteroidMap
      width = length $ head $ asteroidMap
      isAsteroid = (==) '#'
      coords = [(x, y) | y <- [0..height-1], x <- [0..width-1]]
      asteroids = [coord | (coord, c) <- zip coords (concat asteroidMap), isAsteroid c]
      otherAsteroidVectors asteroid = [dist asteroid other | other <- asteroids, other /= asteroid]
      visibleAsteroids asteroid = nubBy sameAngle $ otherAsteroidVectors asteroid
  in
    maximum $ map (length . visibleAsteroids) asteroids

main :: IO ()
main = interact $ show . process . lines
