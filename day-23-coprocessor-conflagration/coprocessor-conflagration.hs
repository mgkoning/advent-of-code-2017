import Data.Char
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List ((\\))
import qualified Data.Set as Set

giev = seq

data State = State {
    programPointer :: Int,
    registers :: Map Char Int,
    mulCount :: Int
  }
  deriving (Show)

emptyState valueA = State 0 (Map.fromList (zip ['a'..'h'] (valueA:(repeat 0)))) 0

data Operand = Register Char | Value Int deriving (Show)
data Operation = Set Operand Operand | 
                 Subtract Operand Operand | 
                 Multiply Operand Operand |
                 JumpNotZero Operand Operand
  deriving (Show)

readInstruction s =
  case op of
    "set" -> Set firstOperand secondOperand
    "sub" -> Subtract firstOperand secondOperand
    "mul" -> Multiply firstOperand secondOperand
    "jnz" -> JumpNotZero firstOperand secondOperand
    where (op:rest) = words s
          firstOperand = readOperand $ head rest
          secondOperand = readOperand $ head $ drop 1 rest

readOperand s
  | head s == '-' && all isDigit (tail s) = Value (negate $ read $ tail s)
  | all isDigit s = Value (read s)
  | length s /= 1 = error "Unknown operand"
  | otherwise = Register $ head s

getValue x state =
  case x of
    Value v -> v
    Register r -> state Map.! r

execute i state =
  case i of
    Set (Register x) y ->
      let valY = getValue y registry
          newRegistry = setValue x valY registry
      in next `giev` state { programPointer = next, registers = newRegistry }
    Subtract (Register x) y -> performOp (-) x y registry
    Multiply (Register x) y ->
      let result = performOp (*) x y registry
          mulCount' = mulCount state + 1
      in mulCount' `giev` result { mulCount = mulCount' }
    JumpNotZero x y ->
      let pointer = (programPointer state)
          jump = pointer + if getValue x registry /= 0 then getValue y registry else 1
      in jump `giev` state { programPointer = jump }
  where
    next = (programPointer state) + 1
    registry = registers state
    performOp op x y registry = 
      let newValue atX = let result = fromMaybe 0 atX `op` getValue y registry
                         in result `giev` Just result
      in next `giev` state { programPointer = next, registers = Map.alter newValue x registry }
    setValue x y registry = Map.alter (\_ -> Just y) x registry

run instructions state 
  | next < 0 || length instructions <= next = state
  | otherwise = run instructions $ execute (instructions !! (programPointer state)) state
  where next = programPointer state

solve = do
  putStrLn "Part 1:"
  instructions <- (map readInstruction . lines) <$> readFile "input.txt"
  let finalState = run instructions (emptyState 0)
  putStrLn $ show $ mulCount $ finalState
  putStrLn "\nPart 2:"
  putStrLn $ show $ length $ findNonPrimes 109900 126900

tryModifiedAlgorithm = do
  instructions2 <- (map readInstruction . lines) <$> readFile "input2.txt"
  let finalState = run instructions2 (emptyState 1)
  putStrLn $ show $ registers $ finalState


{-
  Part 2: The algorithm counts all non-primes in the range [b, b+17 .. c]. For part 2, b is initialized
  to 109900 and c is initialized to 126900.
  The reason it is so slow, is that the algorithm attempts find two factors of n by
  multiplying all numbers in the range [2,n) pair wise and checking for equality with n.
  If equal, a flag is set that increments h (the counter) later. All other permutations are however
  still attempted.
-}
findNonPrimes from to = [from,from+17..to] \\ (dropWhile (<from) (primeSieve to))

primeSieve n = sieve' 3 (Set.fromList (2:[3,5..n]))
  where
    sieve' i set
      | i >= n = Set.toList set
      | not $ Set.member i set = sieve' (i+1) set
      | otherwise = sieve' (i+1) (Set.difference set (Set.fromList [i^2,i^2+i..n]))
