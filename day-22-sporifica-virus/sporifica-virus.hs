import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (Right, Left)

data Direction = Up | Down | Left | Right deriving (Show)
data NodeState = Infected | Clean | Weakened | Flagged deriving (Show, Eq)

giev = seq

turnLeft direction = case direction of Up -> Left; Left -> Down; Down  -> Right; Right -> Up

turnRight direction = case direction of Up -> Right; Right -> Down; Down -> Left; Left  -> Up

reverseDirection direction = case direction of Up -> Down; Down -> Up; Left -> Right; Right -> Left

offset direction =
  case direction of
    Up    -> ( 0, -1)
    Right -> ( 1,  0)
    Down  -> ( 0,  1)
    Left  -> (-1,  0)

nextStateSimple state = case state of Clean -> Infected; Infected -> Clean

nextStateEvolved state = case state of Clean -> Weakened; Weakened -> Infected; Infected -> Flagged; Flagged -> Clean

nextDirection state direction = (case state of Clean -> turnLeft; Weakened -> id; Infected -> turnRight; Flagged -> reverseDirection) direction

readGrid grid = Map.fromList $ concat $ zipWith (\rowIndex row -> zip (zip [0..] (repeat rowIndex)) (map state row)) [0..] (lines grid)
  where
    state c = if c == '#' then Infected else Clean

(|+|) (x0, y0) (x1, y1) = (x0+x1, y0+y1)

burst nextState (grid, pos, dir, infCount) =
  let state = Map.findWithDefault Clean pos grid
      newState = nextState state
      newDirection = nextDirection state dir
      newPos = pos |+| (offset newDirection)
      newInfectionCount = infCount + if newState == Infected then 1 else 0
  in newInfectionCount `giev` (Map.alter (\_ -> Just newState) pos grid, newPos, newDirection, newInfectionCount)

runSim grid startPos stateChange iterations = foldl' (\grid _ -> burst stateChange grid) (grid, startPos, Up, 0) [1..iterations]

solve = do
  putStrLn "Part 1:"
  fileGrid <- readGrid <$> readFile "input.txt"
  let (part1Grid, _, _, infCount1) = runSim fileGrid (12,12) nextStateSimple 10000
  putStrLn $ show $ infCount1
  putStrLn "\nPart 2:"
  let (part2Grid, _, _, infCount2) = runSim fileGrid (12,12) nextStateEvolved 10000000
  putStrLn $ show $ infCount2


{- Test functions -}
testSolve1 it = runSim (readGrid testGrid) (1,1) nextStateSimple it
testSolve2 it = runSim (readGrid testGrid) (1,1) nextStateEvolved it
testGrid = "..#\n#..\n..."

runTests = do
  putStrLn $ "70: " ++ (show $ testSolve1 70)
  let (_, _, _, count1) =  testSolve1 10000
  putStrLn $ "10000: " ++ (show count1)

  putStrLn $ "100: " ++ (show $ testSolve2 100)
  let (_, _, _, count2) =  testSolve2 10000000
  putStrLn $ "10000000: " ++ (show count2)