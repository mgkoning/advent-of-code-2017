import Data.List
import Data.Maybe

runSpinlock :: Int -> Int -> ([Int], Int)
runSpinlock stepSize until = runSpinlock' 0 [0] [1..until]
  where
    runSpinlock' :: Int -> [Int] -> [Int] -> ([Int], Int)
    runSpinlock' p b [] = (b, p)
    runSpinlock' p b (v:vs) = newPosition `seq` newBuffer `seq` runSpinlock' newPosition newBuffer vs
      where
        newPosition = ((p + stepSize) `rem` (length b)) + 1
        newBuffer = insertAt newPosition v b

insertAt :: Int -> Int -> [Int] -> [Int]
insertAt i v b =
  let (prefix, suffix) = splitAt i b
  in prefix ++ [v] ++ suffix

runTest = let (b, p) = runSpinlock 3 2017 in b !! (p + 1)

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ let (b, p) = runSpinlock 312 2017 in b !! ((p+1) `rem` length b)

  putStrLn "Part 2:"
  {- Naive - and far too slow.
    let (resultantBuffer, pos) = runSpinlock 312 50000000
    let zeroPos = fromJust $ elemIndex 0 resultantBuffer
    putStrLn $ show $ resultantBuffer !! ((zeroPos+1) `rem` length resultantBuffer)

    Turns out this is actually the Josephus problem in disguise! (It's reversed.)
  -}
  let lastInsertAt1 = calculateInsertPositions 312 50000000
  putStrLn $ show $ lastInsertAt1

calculateInsertPositions stepSize upTo = calculateInsertPositions' 1 0 upTo (0,0)
  where
    calculateInsertPositions' :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
    calculateInsertPositions' n prev remaining soFar
      | remaining <= 0 = soFar
      | otherwise =
        let
          pos = ((prev + stepSize) `rem` n) + 1
          next = n+1
          nextRemaining = remaining-1
          -- using seq to force calculating these to prevent stack overflows. So much time lost on this.
        in pos `seq` next `seq` nextRemaining `seq`
          calculateInsertPositions' next pos nextRemaining (if pos == 1 then (n, pos) else soFar)