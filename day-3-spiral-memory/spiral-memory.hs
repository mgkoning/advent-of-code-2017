import Data.List
import Data.Maybe

distance :: Int -> Int
distance n = (abs x) + (abs y)
  where
    (x, y) = coordinates n

coordinates :: Int -> (Int, Int)
coordinates n
  | n == 1 = (0, 0)
  | otherwise = (x, y)
    where
      nextRoot = ceiling $ sqrt $ fromIntegral n
      nextOddRoot = if rem nextRoot 2 == 0 then nextRoot + 1 else nextRoot
      (x, y) = determineCoordinates nextOddRoot n

determineCoordinates :: Int -> Int -> (Int, Int)
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

firstNumGreaterThan :: Int -> Int
firstNumGreaterThan n = firstNumGreaterThan' n 2 [((0,0),1)]
  where
    firstNumGreaterThan' n current soFar
      | valueForCurrent > n = valueForCurrent
      | otherwise = firstNumGreaterThan' n (current + 1) $ (currentCoordinates, valueForCurrent) : soFar
      where
        currentCoordinates = coordinates current
        valueForCurrent = sum $ map (getNeighborValue soFar) $ getNeighbors currentCoordinates
        getNeighborValue :: [((Int, Int), Int)] -> (Int, Int) -> Int
        getNeighborValue soFar neighbor = sum $ map snd $ maybeToList $ find (\x -> neighbor == fst x) soFar

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (x + dx, y + dy) /= (x,y)]

solution = do
    putStrLn "Test inputs (part 1):"
    putStrLn $ (++) "1: " $ show $ distance 1
    putStrLn $ (++) "12: " $ show $ distance 12 
    putStrLn $ (++) "23: " $ show $ distance 23 
    putStrLn $ (++) "1024: " $ show $ distance 1024
    putStrLn ""
    putStrLn "Part 1:"
    putStrLn $ (++) "325489: " $ show $ distance 325489
    putStrLn ""
    putStrLn "Part 2:"
    putStrLn $ show $ firstNumGreaterThan 325489