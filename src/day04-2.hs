import Data.List.Split (splitOn)
import Data.List (sort, group)

good :: Int -> Bool
good x =
  let s = show x
      sorted = sort s
      grouped = group sorted
      groupSizes = sort [length g | g <- grouped, length g > 1]
  in
    sorted == s && (length grouped) < (length sorted) && head groupSizes == 2

process :: [Int] -> Int
process (l:r:_) = length [x | x <- [l..r], good x]
process _ = error "Parse error"

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "-" input
