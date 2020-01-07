import Data.List.Split (splitOn)
import Data.List.Extra (groupSort)
import qualified Data.Map as M

getPathToSanta :: [(String, String)] -> String -> String -> Int
getPathToSanta connections =
  let conn = M.fromList $ groupSort connections
      pathToSanta = (
        \parent node ->
          let subnodes = [subnode | subnode <- (conn M.! node), subnode /= parent]
              distances = map (pathToSanta node) subnodes
          in
            if node == "SAN" then 0 else
              1 + (minimum $ 1000000000 : distances)
        )
  in
    pathToSanta

findPath :: [(String, String)] -> Int
findPath connections =
  let pathToSanta = getPathToSanta connections
  in
    (pathToSanta "" "YOU") - 2

tuples (a:b:_) = [(a,b), (b,a)]
tuple _ = error "bad format"

main :: IO ()
main = do
  input <- getContents
  print $ findPath $ concatMap (tuples . splitOn ")") $ lines input
