import Data.List.Split (splitOn)
import Data.List (sort, group)

good :: Int -> Bool
good x =
  let s = show x
      sorted = sort s
      grouped = group sorted
  in
    sorted == s && (length grouped) < (length sorted)

process :: [Int] -> Int
process (l:r:_) = length [x | x <- [l..r], good x]
process _ = error "Parse error"

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "-" input
