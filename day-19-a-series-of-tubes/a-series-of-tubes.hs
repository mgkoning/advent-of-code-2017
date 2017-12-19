import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe

makeMap plan = Map.fromList $ filter ((/= ' ') . snd) $ concat $ zipWith (\rowIndex row -> zip (zip [0..] (repeat rowIndex)) row) [0..] (lines plan)

followMap :: (Int, Int) -> (Int, Int) -> Map (Int, Int) Char -> [((Int, Int), Char)] -> [((Int, Int), Char)]
followMap pos dir plan path =
  case valAtStraight of
    Just v -> followMap straight dir plan ((pos, current):path)
    Nothing -> if current /= '+' then ((pos, current):path) else findNextMove
  where
    straight = add pos dir
    current = plan ! pos
    valAtStraight = Map.lookup straight plan
    findNextMove = followMap neighbor neighborDirection plan ((pos, current):path)
    possibleNeighbors = map (\x -> (add pos x, x)) (allowedDirections dir)
    (neighbor, neighborDirection) = justOne $ filter ((`Map.member` plan)  . fst) possibleNeighbors
    
allowedDirections dir = delete (inverse dir) [(0, -1), (0, 1), (-1, 0), (1, 0)] 

justOne a = if null $ tail a then head a else error ("more than one direction: " ++ (show a))

add (a, b) (c, d) = (a+c, b+d)

inverse (a, b) = (negate a, negate b)

isLetter a = 'A' <= a && a <= 'Z'

solve = do
  plan <- makeMap <$> readFile "input.txt"
  let startPoint = (99, 0)
  let path = followMap startPoint (0,1) (plan) []
  putStrLn "Part 1:"
  putStrLn $ filter isLetter $ map snd $ reverse path
  putStrLn "\nPart 2:"
  putStrLn $ show $ length path

{-
     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 

-}

testInput = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "
