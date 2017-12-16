import Data.List
import Data.List.Split
import Data.Maybe

readMoves :: String -> [String]
readMoves = splitOn ","

dance :: [Char] -> [String] -> [Char]
dance dancers moves = foldl' doMove dancers moves

doMove :: [Char] -> String -> [Char]
doMove dancers move =
  case move of
    's':spin -> let rotation = length dancers - read spin in (drop rotation dancers) ++ (take rotation dancers)
    'x':xchange -> let (a:b:[]) = map read (splitOn "/" xchange) in swap dancers a b
    'p':partner ->
       let (a:b:[]) = map (fromJust . (flip elemIndex) dancers) $ map head $ splitOn "/" partner
       in swap dancers a b

swap :: [Char] -> Int -> Int -> [Char]
swap dancers a b = prefix ++ (take 1 suffix) ++ (tail between) ++ (take 1 between) ++ (tail suffix)
  where
    (first, last) = if a < b then (a, b) else (b, a)
    (prefix, rest) = splitAt (first) dancers
    (between, suffix) = splitAt (last-first) rest

solve = do
  moves <- readMoves  <$> readFile "input.txt"
  let initialPosition =  ['a'..'p']
  putStrLn "Part 1:"
  let finalPosition = dance initialPosition moves
  putStrLn $ finalPosition
  putStrLn "\nPart 2:"
  {-
    It turns out the dance is cyclic! Find how long the cycle is, then take a shortcut and find what
    position corresponds with ONE BILLION.
  -}
  let positions = scanl (\d _ -> dance d moves) initialPosition [0..]
  let cycle = initialPosition:(takeWhile (/=initialPosition) (tail positions))
  let effectiveIndex = oNE_BILLION `rem` (length cycle)
  putStrLn $ (show effectiveIndex) ++ ": " ++ (head $ drop effectiveIndex cycle)

oNE_BILLION = 1000000000

testSolve = dance ['a'..'e'] ["s1", "x3/4", "pe/b"]