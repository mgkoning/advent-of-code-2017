import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Numeric

puzzleInput = "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"

inputAsInts :: [Int]
inputAsInts = map read $ splitOn "," puzzleInput

inputAsAscii :: [Int]
inputAsAscii = map ord $ puzzleInput

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

knotHash :: [Int] -> [Int]
knotHash input = result
  where
    startState = [0..255]
    transformResult = foldl' tieKnot startState (transformations input)
    effectiveShift = (sum $ zipWith (+) input [0..]) `rem` length startState
    result = rotateLeft (length startState - effectiveShift) transformResult

knotHashPart2 :: [Int] -> [Int]
knotHashPart2 input =
  let hashInput = input ++ [17, 31, 73, 47, 23]
      hash = knotHash (take (64 * length hashInput) (cycle hashInput))
      denseHash = map (foldl1' xor) (chunksOf 16 hash)
  in denseHash

showHash :: [Int] -> String
showHash = concat . (map (reverse . (take 2) . reverse . ("00"++) . (`showHex` "")))

solve = do
  let hash = knotHash inputAsInts
  putStrLn "Part 1:"
  putStrLn $ show $ product $ take 2 hash

  putStrLn "\nPart 2:"
  let hash2 = knotHashPart2 inputAsAscii
  putStrLn $ showHash hash2