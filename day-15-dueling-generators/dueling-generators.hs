import Data.Bits

lowest16BitsMask = 65535 :: Int

generator :: Int -> Int -> [Int]
generator factor val = nextVal:(generator factor nextVal)
  where
    nextVal = (val * factor) `rem` 2147483647

generatorA :: Int -> [Int]
generatorA = generator 16807

generatorB :: Int -> [Int]
generatorB = generator 48271

duelingGeneratorsStream :: (Int -> Bool) -> (Int -> Bool) -> Int -> Int -> [Bool]
duelingGeneratorsStream criterionA criterionB startA startB =
  zipWith (==)
    (map (.&. lowest16BitsMask) $ filter criterionA $ generatorA startA)
    (map (.&. lowest16BitsMask) $ filter criterionB $ generatorB startB)

alwaysGood :: a -> Bool
alwaysGood _ = True

part1 :: Int -> Int -> Int
part1 startA startB = length $ filter id $ take 40000000 $ duelingGeneratorsStream alwaysGood alwaysGood startA startB

part2 :: Int -> Int -> Int
part2 startA startB = length $ filter id $ take 5000000 $ duelingGeneratorsStream ((==0) . (`rem` 4)) ((==0) . (`rem` 8)) startA startB

solvePart1 = do
  putStrLn "Part 1:"
  putStrLn $ show $ part1 883 879

solvePart2 = do
  putStrLn "Part 2:"
  putStrLn $ show $ part2 883 879
