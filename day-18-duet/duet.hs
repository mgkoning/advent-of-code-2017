import Data.Char
import Data.Map.Strict (Map, empty)
import Data.Maybe
import qualified Data.Map.Strict as Map

data State = State { programPointer :: Int, lastSound :: Int, registers :: Map Char Int, recovered :: [Int] } deriving (Show)
data Operand = Register Char | Value Int deriving (Show)
data Operation = Sound Operand |
                 Set Operand Operand | 
                 Add Operand Operand | 
                 Multiply Operand Operand |
                 Modulo Operand Operand |
                 Recover Operand |
                 JumpGreaterZero Operand Operand
                 deriving (Show)

readInstruction s =
  case op of
    "snd" -> Sound firstOperand
    "set" -> Set firstOperand secondOperand
    "add" -> Add firstOperand secondOperand
    "mul" -> Multiply firstOperand secondOperand
    "mod" -> Modulo firstOperand secondOperand
    "rcv" -> Recover firstOperand
    "jgz" -> JumpGreaterZero firstOperand secondOperand
    where (op:rest) = words s
          firstOperand = readOperand $ head rest
          secondOperand = readOperand $ head $ drop 1 rest

readOperand s
  | head s == '-' && all isDigit (tail s) = Value (negate $ read $ tail s)
  | all isDigit s = Value (read s)
  | length s /= 1 = error "Unknown operand"
  | otherwise = Register $ head s

execute i state =
  case i of
    Sound x -> next `seq` state { programPointer = next, lastSound = getValue x registry }
    Set (Register x) y ->
      next `seq` state { programPointer = next, registers = Map.alter (\_ -> Just $ getValue y registry) x registry }
    Add (Register x) y -> performOp (+) x y registry
    Multiply (Register x) y -> performOp (*) x y registry
    Modulo (Register x) y -> performOp mod x y registry
    JumpGreaterZero x y ->
      let jump = (programPointer state) + if getValue x registry > 0 then getValue y registry else 1
      in jump `seq` state { programPointer = jump }
    Recover x -> if getValue x registry /= 0 then state { recovered = (lastSound state):(recovered state) } else state
  where
    next = (programPointer state) + 1
    registry = registers state
    performOp op x y registry = 
      let newValue atX = let result = fromMaybe 0 atX `op` getValue y registry in result `seq` Just result
      in next `seq` state { programPointer = next, registers = Map.alter newValue x registry }

getValue x state =
  case x of
    Value v -> v
    Register r -> Map.findWithDefault 0 r state

run instructions state 
  | not $ null $ getRecovered = head getRecovered
  | otherwise = run instructions $ execute (instructions !! (programPointer state)) state
  where getRecovered = take 1 (recovered state) 

solve = do
  putStrLn "Part 1:"
  instructions <- (map readInstruction . lines) <$> readFile "input.txt"
  putStrLn $ show $ run instructions (State 0 0 Map.empty [])