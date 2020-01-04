fuelNeeds :: Int -> Int
fuelNeeds mass
  | mass <= 6 = 0
  | otherwise = fuel + fuelNeeds fuel where fuel = mass `div` 3 - 2

main :: IO ()
main = do
  input <- getContents
  print $ sum $ map (fuelNeeds . read) $ lines input
