import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

parsePosition :: String -> [Int]
parsePosition str = map (read . last . (splitOn "=")) $ splitOn "," $ tail $ init str

data PlanetDimension = PlanetDimension { position :: Int
                                       , velocity :: Int
                                       } deriving (Show, Eq)

type Planet = [PlanetDimension]

gravity :: Int -> Int -> Int
gravity a b = case a `compare` b of
  GT -> -1
  LT -> 1
  EQ -> 0

simPlanetDimension :: [PlanetDimension] -> PlanetDimension -> PlanetDimension
simPlanetDimension pds (PlanetDimension pos vel) =
  let gravity' = sum $ map (gravity pos) $ map position pds
      velocity' = vel + gravity'
      position' = pos + velocity'
  in
    PlanetDimension position' velocity'

simulateDimension :: [PlanetDimension] -> [PlanetDimension]
simulateDimension pds =
  map (simPlanetDimension pds) pds

makePlanetDimension :: [Int] -> PlanetDimension
makePlanetDimension (pos:vel:_) = PlanetDimension pos vel
makePlanetDimension _ = error "unexpected planet dimension input"

makePlanetDimensions :: [Int] -> Planet
makePlanetDimensions pos = map makePlanetDimension $ transpose [pos, replicate (length pos) 0]

simulatePrevious :: (Int, [PlanetDimension]) -> (Int, [PlanetDimension])
simulatePrevious (it, cur) = (it + 1, simulateDimension cur)

findCycleLength :: [PlanetDimension] -> Int
findCycleLength pds =
  let stateEquality = (\(_, cur) -> pds == cur)
      fixedPoint = find stateEquality $ tail $ iterate simulatePrevious (0, pds)
  in
    case fixedPoint of
      Just (it, _) -> it
      Nothing -> 0

process :: [String] -> Int
process positionStrings =
  let positions = map parsePosition positionStrings
      planets = map makePlanetDimensions positions
      cycles = map findCycleLength $ transpose planets
  in
    foldl1 lcm cycles

main :: IO ()
main = interact $ show . process . lines
