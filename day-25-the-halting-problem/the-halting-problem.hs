import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Program = Map Char [StepSpec]
type StepSpec = (Int, Int, Char) -- (write, move, nextState)
type Tape = Map Int Int
type State = (Tape, Int, Char)

giev = seq

program :: Program
program = let left = -1; right = 1 in Map.fromList
  [
    ('A', [(1, right, 'B'), (0, left, 'B')]),
    ('B', [(0, right, 'C'), (1, left, 'B')]),
    ('C', [(1, right, 'D'), (0, left, 'A')]),
    ('D', [(1, left,  'E'), (1, left, 'F')]),
    ('E', [(1, left,  'A'), (0, left, 'D')]),
    ('F', [(1, right, 'A'), (1, left, 'E')])
  ]

update :: Program -> State -> State
update program (tape, pos, currentState) =
  let val = Map.findWithDefault 0 pos tape
      stepSpecs = program Map.! currentState
      (write, move, nextState) = stepSpecs !! val
      tape' = Map.alter (\_ -> Just write) pos tape
      pos' = pos + move
  in tape' `giev` pos' `giev` (tape', pos', nextState)

run :: Program -> Int -> Tape
run program numSteps = run' (Map.empty, 0, 'A') numSteps
  where
    run' :: State -> Int -> Tape
    run' (tape, _, _) 0 = tape
    run' state left =
      let left' = left - 1
      in left' `giev` run' (update program state) left'

solve = do
  putStrLn "Checksum:"
  let result = run program 12629077
  putStrLn $ show $ sum result

testSolve = do
  putStrLn "Checksum:"
  putStrLn $ show $ let result = run testProgram 6 in sum result

testProgram = let left = -1; right = 1 in Map.fromList
  [
    ('A', [(1, right, 'B'), (0, left, 'B')]),
    ('B', [(1, left, 'A'), (1, right, 'A')])
  ]
