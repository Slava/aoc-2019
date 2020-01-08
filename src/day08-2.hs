import Data.List.Split (chunksOf)
import Data.List.Extra (trim, find)
import Data.Maybe (fromMaybe)
import Data.Map (fromList, (!))

process :: String -> [String]
process pixels =
  let width = 25
      height = 6
      layers = chunksOf height $ chunksOf width pixels
      notTransparent = (/= '2')
      applyLayersForPixel = fromMaybe '2' . find notTransparent
      stackedPixels = [applyLayersForPixel (map ((!! x) . (!! y)) layers) | y <- [0..height-1], x <- [0..width-1]]
      colors = fromList [('0', ' '), ('1', 'x')]
      color = (Data.Map.!) colors
      stackedImage = chunksOf width $ map color stackedPixels
  in
    stackedImage

main :: IO ()
main = interact $ unlines . process . trim
