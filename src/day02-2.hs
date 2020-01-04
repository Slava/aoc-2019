import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import           Data.List.Split (splitOn)

executeOpcode :: Int -> Int -> Int -> Int
executeOpcode code
  | code == 1 = (+)
  | code == 2 = (*)

executeProgramWith :: Int -> V.Vector Int -> Int -> V.Vector Int
executeProgramWith opcode memory pc
  | opcode `elem` [1, 2] = let op = executeOpcode opcode
                               src1 = memory ! (pc + 1)
                               src2 = memory ! (pc + 2)
                               dst = memory ! (pc + 3)
                               memory' = memory // [(dst, op (memory ! src1) (memory ! src2))]
                           in
                             executeProgram memory' (pc + 4)
  | opcode == 99 = memory
  | otherwise = error $ "unrecognized opcode at pc: " ++ (show pc)

executeProgram :: V.Vector Int -> Int -> V.Vector Int
executeProgram memory pc = executeProgramWith (memory ! pc) memory pc

runModified :: Int -> Int -> [Int] -> Int
runModified a b program =
  let modifiedProgram = mutableProgram // [(1, a), (2, b)]
      memory = executeProgram modifiedProgram 0
  in
    memory ! 0
  where mutableProgram = V.fromList program

formatAns :: (Int, Int) -> Int
formatAns (a, b) = a * 100 + b

process :: [Int] -> Int
process program =
  formatAns $ head [(a, b) |
                    a <- [0..99],
                    b <- [0..99],
                    runModified a b program == 19690720]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "," input
