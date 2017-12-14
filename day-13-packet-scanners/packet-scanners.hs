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


severity' :: [(Int, Int)] -> Int
severity' = sum . map gateSeverity
  where gateSeverity (pos, depth) = if caughtAt pos depth then pos*depth else 0

everCaught :: [(Int, Int)] -> Int -> Bool
everCaught gates delay = or $ map (\(pos, depth) -> caughtAt (pos+delay) depth) gates

caughtAt :: Int -> Int -> Bool
caughtAt t depth = depth < 2 || t `rem` (2 * depth - 2) == 0

solve = do
  input <- readFile "input.txt"
  let gates = readInput input
  putStrLn "Part 1:"
  putStrLn $ show $ severity gates 0 0
  putStrLn "Part 1 (new):"
  putStrLn $ show $ severity' gates
  putStrLn "\nPart 2 (new):"
  putStrLn $ show $ head $ filter (not . everCaught gates) [1..]
  {-      s       l        o       w. And unnecessarily complicated.
  putStrLn "\nPart 2:"
  putStrLn $ show $ head $ filter ((==0) . severity gates 1) [10..]
  -}

testInput = "0: 3\n1: 2\n4: 4\n6: 4"