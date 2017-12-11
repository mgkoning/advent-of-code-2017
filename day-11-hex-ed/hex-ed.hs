import Data.List
import Data.List.Split

{-
  This solution uses three-axis cube coordinates as described here:
  https://www.redblobgames.com/grids/hexagons/
  One of the values in the coordinates is actually redundant, because in
  that system every hex satisfies x + y + z = 0. However, for ease of implementation
  and understanding, I just used all three.
-}

addTripel :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTripel (a, b, c) (d, e, f) = (a+d, b+e, c+f)

distanceToOrigin :: (Int, Int, Int) -> Int
distanceToOrigin (x, y, z) = (abs x + abs y + abs z) `quot` 2

hexDirection :: String -> (Int, Int, Int)
hexDirection s =
  case s of
    "n"  -> ( 0,  1, -1)
    "ne" -> ( 1,  0, -1)
    "se" -> ( 1, -1,  0)
    "s"  -> ( 0, -1,  1)
    "sw" -> (-1,  0,  1)
    "nw" -> (-1,  1,  0)

distance :: String -> Int
distance = distanceToOrigin . foldl1' addTripel . map hexDirection . splitOn ","

furthestEver :: String -> Int
furthestEver = maximum . map distanceToOrigin . scanl1 addTripel . map hexDirection . splitOn ","

solve = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ distance input
  putStrLn "\nPart 2:"
  putStrLn $ show $ furthestEver input