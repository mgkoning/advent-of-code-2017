import Data.List
import qualified Data.Map.Strict as Map

input :: [Int]
input = map read $ words "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11"

testInput :: [Int]
testInput = [0, 2, 7, 0]

detectLoop :: [Int] ->  (Int, ([Int], Int))
detectLoop = detectLoop' 0 Map.empty
  where
    detectLoop' :: Int -> Map.Map Int [([Int], Int)] -> [Int] -> (Int, ([Int], Int))
    detectLoop' steps cache banks
      | Map.member firstBank cache && not (null foundBanks) = (steps, head foundBanks)
      | otherwise = (detectLoop' $! (steps+1)) (Map.insertWith (++) firstBank [(banks, steps)] cache) newBanks
      where
        foundBanks = filter ((banks==) . fst) (cache Map.! firstBank)
        firstBank = head banks
        newBanks = updateBanks banks

updateBanks :: [Int] -> [Int]
updateBanks banks = zipWith (+) banksWithoutMax redivided
  where
    numberedBanks = zip [1..] banks
    (maxIndex, maxValue) = maximumBy firstMaximum numberedBanks
    banksWithoutMax = setZeroAt maxIndex banks
    redivided = rotateRight maxIndex (distribute maxValue (length banks))

setZeroAt :: Int -> [Int] -> [Int]
setZeroAt index list = (take (index-1) list) ++ [0] ++ (drop index list)

firstMaximum :: (Int, Int) -> (Int, Int) -> Ordering
firstMaximum (a1, b1) (a2, b2)
  | b1 == b2 = compare a2 a1
  | otherwise = compare b1 b2

rotateRight :: Int -> [a] -> [a]
rotateRight n x = drop (length x - n) x ++ take (length x - n) x

distribute :: Int -> Int -> [Int]
distribute val len = zipWith (+) (replicate len q) $ (replicate r 1) ++ (replicate (len-r) 0)
  where
    (q, r) = quotRem val len

solution = do
  putStrLn $ "Test input: " ++ (show $ detectLoop testInput)
  let (stepsTaken, (banks, previouslySeenAt)) = detectLoop input
  putStrLn $ "Part 1: " ++ (show stepsTaken)
  putStrLn $ "Part 2: " ++ (show $ stepsTaken - previouslySeenAt)
