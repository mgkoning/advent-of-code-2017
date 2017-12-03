import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

-- Manhattan distance to origin for the given grid 'index'. Calculates coordinates for index and takes absolute
-- value of both components.
distance :: Integer -> Integer
distance n = (abs x) + (abs y)
  where
    (x, y) = coordinates n

-- Returns coordinates on the grid based on the (1-based) index given, where 1 equals (0, 0)
-- and the grid spirals counterclockwise.
coordinates :: Integer -> (Integer, Integer)
coordinates n
  | n == 1 = (0, 0)
  | otherwise = (x, y)
    where
      -- Root calculation is done because the bottom right value of every square in the spiral is
      -- always second power of an odd number (1, 9, 25, ...). This gives a good start point for
      -- the search: the actual 'square' in the spiral can be determined this way.
      nextRoot = ceiling $ sqrt $ fromIntegral n
      nextOddRoot = if rem nextRoot 2 == 0 then nextRoot + 1 else nextRoot
      (x, y) = determineCoordinates nextOddRoot n

determineCoordinates :: Integer -> Integer -> (Integer, Integer)
determineCoordinates nextOddRoot n
  | diff <= sideSize = (fst bottomRightCoordinates, snd bottomRightCoordinates + diff)
  | diff <= 2 * sideSize = (fst bottomRightCoordinates - (diff - sideSize), snd topLeftCoordinates)
  | diff <= 3 * sideSize = (fst topLeftCoordinates, snd topLeftCoordinates - (diff - 2 * sideSize))
  | otherwise = (fst topLeftCoordinates + (diff - 3*sideSize), snd bottomRightCoordinates)
  where
    sideSize = nextOddRoot - 1
    bottomRightCoordinates = (quot nextOddRoot 2, -1 * quot nextOddRoot 2)
    topLeftCoordinates = (snd bottomRightCoordinates, fst bottomRightCoordinates)
    diff = n - ((nextOddRoot-2) ^ 2)

-- Walks through all values generated in the grid to find the first one larger than n.
firstNumGreaterThan :: Integer -> (Integer, (Integer, Integer))
firstNumGreaterThan n = firstNumGreaterThan' n 2 $ Map.singleton (0,0) 1
  where
    -- Helper function that recursively accumulates all calculations into a map of known values.
    firstNumGreaterThan' :: Integer -> Integer -> Map.Map (Integer, Integer) Integer -> (Integer, (Integer, Integer))
    firstNumGreaterThan' n current soFar
      | valueForCurrent > n = (valueForCurrent, currentCoordinates)
      | otherwise = firstNumGreaterThan' n (current + 1) $ Map.insert currentCoordinates valueForCurrent soFar
      where
        currentCoordinates = coordinates current
        valueForCurrent = sum $ map (getNeighborValue soFar) $ getNeighbors currentCoordinates
        getNeighborValue :: Map.Map (Integer, Integer) Integer -> (Integer, Integer) -> Integer
        getNeighborValue soFar neighbor = Map.findWithDefault 0 neighbor soFar

-- getNeighbors returns all neighbors of the given coordinate.
getNeighbors :: (Integer, Integer) -> [(Integer, Integer)]
getNeighbors (x, y) = [(x', y')
  | dx <- [-1..1],
    dy <- [-1..1],
    let (x', y') = (x + dx, y + dy),
    (x', y') /= (x, y) -- do not return input as neighbor
  ]

solution = do
    putStrLn "Test inputs (part 1):"
    putStrLn $ (++) "1: " $ show $ distance 1
    putStrLn $ (++) "12: " $ show $ distance 12
    putStrLn $ (++) "23: " $ show $ distance 23
    putStrLn $ (++) "1024: " $ show $ distance 1024
    putStrLn "\nPart 1:"
    putStrLn $ (++) "325489: " $ show $ distance 325489
    putStrLn "\nPart 2:"
    putStrLn $ show $ fst $ firstNumGreaterThan $ 325489
    putStrLn $ "\nBonus (big numbers):"
    putStrLn $ show $ firstNumGreaterThan $ 4567476145634564545645454562348890634324235432482375348957893473896789576897548967453768457689745893678934576897458968945768945768974589768945768934576897456789345892739587727658746235786234785678346578346257863478563465786347856783465723479582389048238189
    