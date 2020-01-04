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

process :: [Int] -> Int
process program =
  let modifiedProgram = mutableProgram // [(1, 12), (2, 2)]
      memory = executeProgram modifiedProgram 0
  in
    memory ! 0
  where mutableProgram = V.fromList program

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "," input
