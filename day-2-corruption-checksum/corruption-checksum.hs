import Data.List
import Data.Maybe

solution :: IO()
solution = do
    contents <- readFile "input.txt"
    putStrLn $ "Solution part 1: " ++ (show $ checksum minMaxChecksummer contents)
    putStrLn $ "Solution part 2: " ++ (show $ checksum divisibleChecksummer contents)

checksum :: ([Int] -> Int) -> String -> Int
checksum rowChecksummer contents = sum $ map rowChecksummer $ spreadsheetContents contents

spreadsheetContents :: String -> [[Int]]
spreadsheetContents contents = map ((map read) . words) (lines contents)

minMaxChecksummer :: [Int] -> Int
minMaxChecksummer [] = error "No row contents"
minMaxChecksummer xs = let sorted = sort xs in (last sorted) - (head sorted)

divisibleChecksummer :: [Int] -> Int
divisibleChecksummer [] = error "Not enough values"
divisibleChecksummer [_] = error "Not enough values"
divisibleChecksummer xs = head $ mapMaybe compareCells [(x, y) | (x:rest) <- tails xs, y <- rest]
  where
    compareCells :: (Int, Int) -> Maybe Int
    compareCells (x, y)
      | rem x y == 0 = Just (quot x y)
      | rem y x == 0 = Just (quot y x)
      | otherwise = Nothing

testInput = "5 1 9 5\n7 5 3\n2 4 6 8"
testInput2 = "5 9 2 8\n9 4 7 3\n3 8 6 5"