import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import           Data.List.Split (splitOn)

executeOpcode :: Int -> Int -> Int -> Int
executeOpcode code
  | code == 1 = (+)
  | code == 2 = (*)

parseSubcode :: Int -> Int
parseSubcode code = code `rem` 100

parseOpcode :: Int -> (Int, Bool, Bool, Bool)
parseOpcode code = (
  parseSubcode code,
  code `div` 100 `rem` 10 /= 0,
  code `div` 1000 `rem` 10 /= 0,
  code `div` 10000 `rem` 10 /= 0
  )

isBin :: Int -> Bool
isBin opcode = (parseSubcode opcode) `elem` [1, 2]

executeProgramWith :: Int -> V.Vector Int -> [Int] -> Int -> [Int]
executeProgramWith opcode memory input pc
  | isBin opcode =
    let (code, immediate1, immediate2, _) = parseOpcode opcode
        op = executeOpcode code
        src1 = memory ! (pc + 1)
        src2 = memory ! (pc + 2)
        dst = memory ! (pc + 3)
        operand1 = if immediate1 then src1 else memory ! src1
        operand2 = if immediate2 then src2 else memory ! src2
        memory' = memory // [(dst, op operand1 operand2)]
    in
       executeProgram memory' input (pc + 4)
  | opcode == 3 =
    let dst = memory ! (pc + 1)
        inp = head input
        input' = tail input
        memory' = memory // [(dst, inp)]
    in
      executeProgram memory' input' (pc + 2)
  | parseSubcode opcode == 4 =
    let (_, immediate, _, _) = parseOpcode opcode
        src = memory ! (pc + 1)
        outp = if immediate then src else memory ! src
    in
      [outp] ++ executeProgram memory input (pc + 2)
  | opcode == 99 = []
  | otherwise = error $ "unrecognized opcode at pc: " ++ (show pc)

executeProgram :: V.Vector Int -> [Int] -> Int -> [Int]
executeProgram memory input pc = executeProgramWith (memory ! pc) memory input pc

run :: [Int] -> [Int] -> [Int]
run program input =
  let mutableProgram = V.fromList program
      output = executeProgram mutableProgram input 0
  in
    output

process :: [Int] -> [Int]
process program =
  run program [1]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "," input
