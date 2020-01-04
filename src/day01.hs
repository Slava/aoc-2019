process :: Int -> Int
process x = x `div` 3 - 2

main :: IO ()
main = do
  input <- getContents
  print $ sum $ map (process . read) $ lines input
