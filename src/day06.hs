import Data.List.Split (splitOn)
import Data.Set (toList, fromList)

uniquify lst = toList $ fromList lst

checksum :: [(String, String)] -> Int
checksum orbits =
  let planets = uniquify $ concatMap (\o -> [fst o, snd o]) orbits
      countOrbits planet =
        let immediateOrbits = [to | (from, to) <- orbits, from == planet]
            subValues = map countOrbits immediateOrbits
            subOrbits = map fst subValues
            subSizes = map snd subValues
            size = 1 + sum subSizes
            count = sum subSizes + sum subOrbits
        in
          (count, size)
  in
    maximum $ map (fst . countOrbits) planets

tuple :: [String] -> (String, String)
tuple (a:b:_) = (a,b)
tuple _ = error "bad format"

main :: IO ()
main = do
  input <- getContents
  print $ checksum $ map (tuple . splitOn ")") $ lines input
