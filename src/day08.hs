import Data.Map (fromListWith, lookup, Map)
import Data.List.Split (chunksOf)
import Data.List.Extra (minimumBy, trim)
import Data.Maybe (fromMaybe)

frequency :: (Ord a) => [a] -> Map a Int
frequency xs = fromListWith (+) [(x, 1) | x <- xs]

process :: String -> Int
process pixels =
  let width = 25
      height = 6
      layers = chunksOf height $ chunksOf width pixels
      countByLayer = map (frequency . concat) layers
      lookupDefault k m = fromMaybe 0 (Data.Map.lookup k m)
      lookup0 = lookupDefault '0'
      smallestLayer = minimumBy (\a b -> compare (lookup0 a) (lookup0 b)) countByLayer
  in
    (lookupDefault '1' smallestLayer) * (lookupDefault '2' smallestLayer)

main :: IO ()
main = interact $ show . process . trim
