import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import           Data.List.Split (splitOn)
import Data.List (permutations)

executeOpcode :: Int -> Int -> Int -> Int
executeOpcode code
  | code == 1 = (+)
  | code == 2 = (*)
  | code == 7 = \a b -> if a < b then 1 else 0
  | code == 8 = \a b -> if a == b then 1 else 0

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
isBin opcode = (parseSubcode opcode) `elem` [1, 2, 7, 8]

getValue :: Int -> Bool -> V.Vector Int -> Int
getValue val immediate memory = if immediate then val else memory ! val

executeProgramWith :: Int -> V.Vector Int -> [Int] -> Int -> [Int]
executeProgramWith opcode memory input pc
  | isBin opcode =
    let (code, immediate1, immediate2, _) = parseOpcode opcode
        op = executeOpcode code
        src1 = memory ! (pc + 1)
        src2 = memory ! (pc + 2)
        dst = memory ! (pc + 3)
        operand1 = getValue src1 immediate1 memory
        operand2 = getValue src2 immediate2 memory
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
        outp = getValue src immediate memory
    in
      [outp] ++ executeProgram memory input (pc + 2)
  | parseSubcode opcode == 5 || parseSubcode opcode == 6 =
    let (code, immediate1, immediate2, _) = parseOpcode opcode
        src = memory ! (pc + 1)
        dst = memory ! (pc + 2)
        srcVal = getValue src immediate1 memory
        dstVal = getValue dst immediate2 memory
        branchValue = code == 5
    in
      if (srcVal /= 0) == branchValue
      then executeProgram memory input dstVal
      else executeProgram memory input (pc + 3)
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

process :: [Int] -> Int
process program =
  let runCycles (phase1:phase2:phase3:phase4:phase5:_) =
        let output1 = run program $ phase1 : 0 : output5
            output2 = run program $ phase2 : output1
            output3 = run program $ phase3 : output2
            output4 = run program $ phase4 : output3
            output5 = run program $ phase5 : output4
        in
          last output5
      runCycles _ = error "5 phases required"
  in
    maximum $ map runCycles $ permutations [5..9]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ splitOn "," input
