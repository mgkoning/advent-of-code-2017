import Data.Bits

lowest16BitsMask = 65535

generator :: Int -> Int -> [Int]
generator factor val = nextVal:(generator factor nextVal)
  where
    nextVal = (val * factor) `rem` 2147483647

generatorA = generator 16807
generatorB = generator 48271

duelingGeneratorsStream criterionA criterionB startA startB =
  zipWith (==)
    (map (.&. lowest16BitsMask) $ filter criterionA $ generatorA startA)
    (map (.&. lowest16BitsMask) $ filter criterionB $ generatorB startB)

alwaysGood _ = True

part1 startA startB = length $ filter id $ take 40000000 $ duelingGeneratorsStream alwaysGood alwaysGood startA startB

part2 startA startB = length $ filter id $ take 5000000 $ duelingGeneratorsStream ((==0) . (`rem` 4)) ((==0) . (`rem` 8)) startA startB

solvePart1 = do
  putStrLn "Part 1:"
  putStrLn $ show $ part1 883 879

solvePart2 = do
  putStrLn "Part 2:"
  putStrLn $ show $ part2 883 879
