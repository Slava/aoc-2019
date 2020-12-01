import Data.List.Split (splitOn)
import Data.List.Extra (trim, groupOn, find)
import Debug.Trace (traceShow)

type Req = (String, Int)
type Reaction = ([Req], Req)

parseReq :: String -> Req
parseReq str =
  let (amt:label:_) = map trim $ splitOn " " $ trim str
  in (label, read amt)

parseReaction :: String -> Reaction
parseReaction str =
  let (lhs:rhs:_) = splitOn "=>" str
      inputs = map parseReq $ splitOn "," lhs
      output = parseReq rhs
  in
    (inputs, output)

expand :: [Reaction] -> Req -> [Req]
expand expansions (label, amount) = case find ((label ==) . fst . snd) expansions of
  Just (reqs, (_, rxAmount)) -> map (\(l, a) -> (l, (amount + rxAmount - 1) `div` rxAmount * a)) reqs
  Nothing -> if label == "ORE" then [(label, amount)] else error $ "No reaction found for " ++ label

combineReqs :: [Req] -> Req
combineReqs reqs = (fst (head reqs), sum (map snd reqs))

reduceToOre :: [Reaction] -> [Req] -> Int
reduceToOre expansions targets  =
  let expanded = concatMap (expand expansions) targets
      grouped = groupOn fst expanded
      collapsed = map combineReqs grouped
  in
    if length collapsed == 1 && (fst . head) collapsed == "ORE" then (snd . head) collapsed
    else reduceToOre expansions collapsed

process :: String -> String
process input = show $ flip reduceToOre [("FUEL", 1)] $ map parseReaction $ lines input

main :: IO ()
main = interact process
