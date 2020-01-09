import Data.IntMap (IntMap, lookup, insert, fromAscList)
import qualified Data.Map as M
import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type Memory = IntMap Int

executeOpcode :: Int -> Int -> Int -> Int
executeOpcode code
  | code == 1 = (+)
  | code == 2 = (*)
  | code == 7 = \a b -> if a < b then 1 else 0
  | code == 8 = \a b -> if a == b then 1 else 0
  | otherwise = error "Invalid bin operation code"

parseSubcode :: Int -> Int
parseSubcode code = code `rem` 100

parseOpcode :: Int -> (Int, Int, Int, Int)
parseOpcode code = (
  parseSubcode code,
  code `div` 100 `rem` 10,
  code `div` 1000 `rem` 10,
  code `div` 10000 `rem` 10
  )

isBin :: Int -> Bool
isBin opcode = (parseSubcode opcode) `elem` [1, 2, 7, 8]

getValue :: Int -> Int -> Int -> Memory -> Int
getValue bp val mode memory = case mode of
  0 -> readFromMemory val memory
  1 -> val
  2 -> readFromMemory (bp + val) memory
  _ -> error "Invalid mode"

getDest :: Int -> Int -> Int -> Int
getDest _ val 0 = val
getDest bp val 2 = bp + val
getDest _ _ 1 = error "Immediate mode used with store"
getDest _ _ _ = error "Invalid mode"

updateMemory :: Int -> Int -> Memory -> Memory
updateMemory = insert

readFromMemory :: Int -> Memory -> Int
readFromMemory pos mem = fromMaybe 0 $ Data.IntMap.lookup pos mem

executeProgramWith :: Int -> Memory -> [Int] -> (Int, Int) -> [Int]
executeProgramWith opcode memory input (pc, bp)
  | isBin opcode =
    let (code, mode1, mode2, modeDst) = parseOpcode opcode
        op = executeOpcode code
        src1 = readFromMemory (pc + 1) memory
        src2 = readFromMemory (pc + 2) memory
        dst = readFromMemory (pc + 3) memory
        operand1 = getValue bp src1 mode1 memory
        operand2 = getValue bp src2 mode2 memory
        memory' = updateMemory (getDest bp dst modeDst) (op operand1 operand2) memory
    in
       executeProgram memory' input (pc + 4, bp)
  | parseSubcode opcode == 3 =
    let (_, modeDst, _, _) = parseOpcode opcode
        dst = readFromMemory (pc + 1) memory
        inp = head input
        input' = tail input
        memory' = updateMemory (getDest bp dst modeDst) inp memory
    in
      executeProgram memory' input' (pc + 2, bp)
  | parseSubcode opcode == 4 =
    let (_, mode, _, _) = parseOpcode opcode
        src = readFromMemory (pc + 1) memory
        outp = getValue bp src mode memory
    in
      [outp] ++ executeProgram memory input (pc + 2, bp)
  | parseSubcode opcode == 5 || parseSubcode opcode == 6 =
    let (code, mode1, mode2, _) = parseOpcode opcode
        src = readFromMemory (pc + 1) memory
        dst = readFromMemory (pc + 2) memory
        srcVal = getValue bp src mode1 memory
        dstVal = getValue bp dst mode2 memory
        branchValue = code == 5
    in
      if (srcVal /= 0) == branchValue
      then executeProgram memory input (dstVal, bp)
      else executeProgram memory input (pc + 3, bp)
  | parseSubcode opcode == 9 =
    let (_, mode, _, _) = parseOpcode opcode
        src = readFromMemory (pc + 1) memory
        operand = getValue bp src mode memory
    in
      executeProgram memory input (pc + 2, bp + operand)
  | opcode == 99 = []
  | otherwise = error $ "unrecognized opcode at pc: " ++ (show pc) ++ " code: " ++ (show opcode)

executeProgram :: Memory -> [Int] -> (Int, Int) -> [Int]
executeProgram memory input (pc, bp) = executeProgramWith (readFromMemory pc memory) memory input (pc, bp)

run :: [Int] -> [Int] -> [Int]
run program input =
  let mutableProgram = fromAscList $ zip [0..(length program)-1] program
      output = executeProgram mutableProgram input (0, 0)
  in
    output

data Orientation = Up | Down | Lft | Rght
type Board = M.Map (Int, Int) Int

makeTurn :: Orientation -> Int -> Orientation
makeTurn Up 0 = Lft
makeTurn Up 1 = Rght
makeTurn Lft 0 = Down
makeTurn Lft 1 = Up
makeTurn Down 0 = Rght
makeTurn Down 1 = Lft
makeTurn Rght 0 = Up
makeTurn Rght 1 = Down
makeTurn _ _ = error "unknown turn instruction"

makeStep :: (Int, Int) -> Orientation -> (Int, Int)
makeStep (x, y) Up = (x, y - 1)
makeStep (x, y) Lft = (x - 1, y)
makeStep (x, y) Down = (x, y + 1)
makeStep (x, y) Rght = (x + 1, y)

runPainter :: Board -> (Int, Int) -> Orientation -> [Int] -> [(((Int, Int), Int), Int)]
runPainter _ _ _ [] = []
runPainter state pos ort (color:turn:rest) =
  let nextOrt = makeTurn ort turn
      nextPos = makeStep pos nextOrt
      nextState = M.insert pos color state
      nextColor = M.findWithDefault 0 nextPos state
  in
    ((pos, color), nextColor) : runPainter nextState nextPos nextOrt rest

runPainter _ _ _ _ = error "unexpected end of input"

process :: [Int] -> String
process program =
  let robotInput = map snd painterOutput
      robotOutput = run program robotInput
      painterOutput = (((0, 0), 1), 1) : (runPainter (M.singleton (0, 0) 1) (0, 0) Up robotOutput)
      maxX = maximum $ map (fst . fst . fst) painterOutput
      maxY = maximum $ map (snd . fst . fst) painterOutput
      minX = minimum $ map (fst . fst . fst) painterOutput
      minY = minimum $ map (snd . fst . fst) painterOutput
      applied = M.fromList $ map fst painterOutput
      toPrint c = if c == 0 then '.' else '#'
      board = chunksOf (maxX - minX + 1) $ [toPrint (M.findWithDefault 0 (x, y) applied) | y <- [minY..maxY], x <- [minX..maxX]]
  in
    unlines board

main :: IO ()
main = interact $ process . map read . splitOn ","
