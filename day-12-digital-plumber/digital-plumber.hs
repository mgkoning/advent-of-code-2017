import Data.Set (Set, empty, member, insert, size, foldl, toList)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), fromList, keys)
import Data.List (foldl')

readNeighboursMap :: String -> Map String [String]
readNeighboursMap s = fromList $ map makeTuple $ map (splitOn " <-> ") $ lines s
  where
    makeTuple :: [String] -> (String, [String])
    makeTuple xs = (head xs, splitOn ", " (head $ drop 1 xs))

reachableFrom :: Map String [String] -> String -> Set String
reachableFrom map startPoint = reachableFrom' empty [startPoint]
  where
    reachableFrom' :: Set String -> [String] -> Set String
    reachableFrom' soFar [] = soFar
    reachableFrom' soFar (x:xs)
      | member x soFar = reachableFrom' soFar xs
      | otherwise = reachableFrom' (insert x soFar) (xs ++ (map ! x))

solve = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  let neighboursMap = readNeighboursMap input
  let reachableFrom0 = reachableFrom neighboursMap "0"
  putStrLn $ show $ size reachableFrom0
  putStrLn "Part 2:"
  let groups = foldl' addToGroups [reachableFrom0] (keys neighboursMap)
      addToGroups :: [Set String] -> String -> [Set String]
      addToGroups groups x = if any (member x) groups then groups else (reachableFrom neighboursMap x):groups
  putStrLn $ show $ length groups