import Data.Maybe

readInput :: String -> [(Int, Int)]
readInput = (map parseLine) . lines
  where parseLine s = (read $ takeWhile (/=':') s, read $ tail $ dropWhile (/=':') s)

severity :: [(Int, Int)] -> Int -> Int -> Int
severity gates startScoreAt delay = sum $ map score [0..maxGate]
  where
    maxGate = fst (last gates)
    score t =
      case lookup t gates of
        Nothing -> 0
        Just range -> if caught then (t + startScoreAt) * range else 0
          where caught = 0 == positionAt (t+delay) range

positionAt :: Int -> Int -> Int
positionAt t depth = head $ drop (t `rem` (length gateCycle)) $ gateCycle
  where gateCycle = [0..depth-1] ++ (reverse [1..depth-2])

solve = do
  input <- readFile "input.txt"
  let gates = readInput input
  putStrLn "Part 1:"
  putStrLn $ show $ severity gates 0 0
  putStrLn "\nPart 2:"
  putStrLn $ show $ head $ filter ((==0) . severity gates 1) [10..]

testInput = "0: 3\n1: 2\n4: 4\n6: 4"