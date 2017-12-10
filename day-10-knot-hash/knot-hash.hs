import Data.List
import Data.List.Split

input :: [Int]
input = map read $ splitOn "," "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"

transformations :: [Int] -> [(Int, Int)]
transformations input = zip input [0..]

tieKnot :: [Int] -> (Int, Int) -> [Int]
tieKnot state (stepLength, skipSize) =
  let
    transform = (reverse $ take stepLength state) ++ drop stepLength state
    shift = rotateLeft (stepLength + skipSize) transform
  in shift

rotateLeft :: Int -> [Int] -> [Int]
rotateLeft n xs = take (length xs) $ drop n (cycle xs)

knotHash :: [Int] -> [Int] -> [Int]
knotHash input startState = result
  where
    transformResult = foldl' tieKnot startState (transformations input)
    effectiveShift = (sum $ zipWith (+) input [0..]) `rem` length startState
    result = rotateLeft (length startState - effectiveShift) transformResult

solve = do
  let hash = knotHash input [0..255]
  putStrLn $ show $ product $ take 2 hash