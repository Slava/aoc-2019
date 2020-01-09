import Data.List.Split (splitOn)
import Debug.Trace (trace)

parsePosition :: String -> [Int]
parsePosition str = map (read . last . (splitOn "=")) $ splitOn "," $ tail $ init str

data Planet = Planet { position :: [Int]
                     , velocity :: [Int]
                     , energy :: Int
                     } deriving Show

gravity :: Int -> Int -> Int
gravity a b = case a `compare` b of
  GT -> -1
  LT -> 1
  EQ -> 0

addVectors :: [Int] -> [Int] -> [Int]
addVectors a b = map (uncurry (+)) $ zip a b

vecGravity :: [Int] -> [Int] -> [Int]
vecGravity pos other = map (uncurry gravity) $ zip pos other

simPlanet :: [Planet] -> Planet -> Planet
simPlanet ps (Planet pos vel _) =
  let gravity' = foldl1 addVectors $ map (vecGravity pos) $ map position ps
      velocity' = addVectors vel gravity'
      position' = addVectors pos velocity'
  in
    Planet position' velocity' ((sum (map abs position')) * (sum (map abs velocity')))

simulate :: [Planet] -> [Planet]
simulate planets =
  map (simPlanet planets) planets

process :: [String] -> Int
process positionStrings =
  let positions = map parsePosition positionStrings
      initPlanet pos = Planet pos [0, 0, 0] 0
      planets = map initPlanet positions
  in
    sum $ map energy $ last $ take 1001 $ iterate simulate planets

main :: IO ()
main = interact $ show . process . lines
