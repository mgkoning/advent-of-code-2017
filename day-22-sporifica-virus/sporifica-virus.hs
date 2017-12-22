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

testSolve1 = foldl' (\grid _ -> burst nextStateSimple grid) (readGrid testGrid, (1,1), Up, 0) [1..70]
testGrid = "..#\n#..\n..."

runSim grid stateChange iterations = foldl' (\grid _ -> burst stateChange grid) (grid, (12,12), Up, 0) [1..iterations]

solve = do
  putStrLn "Part 1:"
  fileGrid <- readGrid <$> readFile "input.txt"
  let (part1Grid, _, _, infCount1) = runSim fileGrid nextStateSimple 10000
  putStrLn $ show $ infCount1
  putStrLn "\nPart 2:"
  let (part2Grid, _, _, infCount2) = runSim fileGrid nextStateEvolved 10000000
  putStrLn $ show $ infCount2