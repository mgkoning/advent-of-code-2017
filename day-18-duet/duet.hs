import Data.Char
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map

data State = State {
    programPointer :: Int,
    outbox :: [Int],
    registers :: Map Char Int,
    blocked :: Bool,
    inbox :: [Int],
    sent :: Int
  }
  deriving (Show)

emptyState = State 0 [] Map.empty False [] 0

data Operand = Register Char | Value Int deriving (Show)
data Operation = Send Operand |
                 Set Operand Operand | 
                 Add Operand Operand | 
                 Multiply Operand Operand |
                 Modulo Operand Operand |
                 Receive Operand |
                 JumpGreaterZero Operand Operand
  deriving (Show)

readInstruction s =
  case op of
    "snd" -> Send firstOperand
    "set" -> Set firstOperand secondOperand
    "add" -> Add firstOperand secondOperand
    "mul" -> Multiply firstOperand secondOperand
    "mod" -> Modulo firstOperand secondOperand
    "rcv" -> Receive firstOperand
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
    Send x -> let val = getValue x registry
                  newSent = 1 + (sent state)
              in val `seq` next `seq` newSent `seq`
                 state { programPointer = next, outbox = val:(outbox state), sent = newSent }
    Set (Register x) y ->
              let valY = getValue y registry
                  newRegistry = setValue x valY registry
              in next `seq` state { programPointer = next, registers = newRegistry }
    Add (Register x) y -> performOp (+) x y registry
    Multiply (Register x) y -> performOp (*) x y registry
    Modulo (Register x) y -> performOp mod x y registry
    JumpGreaterZero x y ->
      let pointer = (programPointer state)
          jump = pointer + if getValue x registry > 0 then getValue y registry else 1
      in jump `seq` state { programPointer = jump }
    Receive (Register x) ->
      if null $ inbox state
        then state { blocked = True }
        else
          let m:ms = inbox state
          in state { programPointer = next, inbox = ms, registers = setValue x m registry }
  where
    next = (programPointer state) + 1
    registry = registers state
    performOp op x y registry = 
      let newValue atX = let result = fromMaybe 0 atX `op` getValue y registry
                         in result `seq` Just result
      in next `seq` state { programPointer = next, registers = Map.alter newValue x registry }
    setValue x y registry = Map.alter (\_ -> Just y) x registry

getValue x state =
  case x of
    Value v -> v
    Register r -> Map.findWithDefault 0 r state

run instructions state 
  | blocked state = state
  | otherwise = run instructions $ execute (instructions !! (programPointer state)) state

stopped instructions state =
  let next = programPointer state
      stuck = blocked state && (null $ outbox state)
  in next < 0 || length instructions <= next || stuck

runPart2 instructions = runPart2' (thread0, thread1)
  where
    thread0 = emptyState { registers = Map.singleton 'p' 0 }
    thread1 = emptyState { registers = Map.singleton 'p' 1 }
    runPart2' (t0, t1)
      | all (stopped instructions) [t0, t1] = (t0, t1)
      | otherwise = runPart2' (newT0, newT1)
        where
          newT0 = run instructions $
            t0 { blocked = False, inbox = reverse $ outbox t1, outbox = [] }
          newT1 = run instructions $
            t1 { blocked = False, inbox = reverse $ outbox t0, outbox = [] }

solve = do
  putStrLn "Part 1:"
  instructions <- (map readInstruction . lines) <$> readFile "input.txt"
  putStrLn $ show $ head $ outbox $ run instructions emptyState
  putStrLn "\nPart 2:"
  let (thread0, thread1) = runPart2 instructions
  putStrLn $ show $ sent thread1