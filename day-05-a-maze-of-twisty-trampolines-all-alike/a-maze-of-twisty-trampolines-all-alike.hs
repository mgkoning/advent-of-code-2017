import Data.Map.Strict

readInstructions :: String -> [Int]
readInstructions input = Prelude.map read $ lines $ input

followUntilOutOfBounds :: (Int -> Int) -> [Int] -> Int
followUntilOutOfBounds update instructions = followUntilOutOfBounds' update (0, length instructions) 0 0 instructionsMap
  where
    instructionsMap = fromList $ zip [0..] instructions

{-
  Does the actual work. ($!) is used in the recursion call, because the accumulated addition expression results in
  a stack overflow.
-}
followUntilOutOfBounds' :: (Int -> Int) -> (Int, Int) -> Int -> Int -> Map Int Int -> Int
followUntilOutOfBounds' update bounds pos stepsTaken instructionsMap
  | pos < fst bounds || pos >= snd bounds = stepsTaken
  | otherwise = (followUntilOutOfBounds' update bounds (pos + (instructionsMap ! pos)) $! (1 + stepsTaken)) (adjust update pos instructionsMap)

part2Update :: Int -> Int
part2Update n
  | n >= 3 = n - 1
  | otherwise = n + 1

solution = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn "Test input:"
  putStrLn $ show $ followUntilOutOfBounds (+1) $ readInstructions exampleInput
  putStrLn "\nActual input:"
  putStrLn $ show $ followUntilOutOfBounds (+1) $ readInstructions input

  putStrLn "\nPart 2:"
  putStrLn "Test input:"
  putStrLn $ show $ followUntilOutOfBounds part2Update $ readInstructions exampleInput
  putStrLn "\nActual input:"
  putStrLn $ show $ followUntilOutOfBounds part2Update $ readInstructions input

exampleInput = "0\n3\n0\n1\n-3"