import Data.Set (Set)
import qualified Data.Set as Set
import KnotHash

-- Generate a string representation of the disk given a key.
disk :: String -> [String]
disk key = map (knotHashToBinString . ((key ++ "-") ++) . show) [0..127]

-- Builds a set of coordinates from the disk key.
diskSet :: String -> Set (Int, Int)
diskSet key = diskSetFromDisk (disk key)

{-
  Builds a set of coordinates from a string representation of the disk. Only coordinates
  that have a value of '1' are relevant, so the other coordinates are discarded.
-}
diskSetFromDisk :: [String] -> Set (Int, Int)
diskSetFromDisk disk = Set.fromList $ map fst $ filter (\((x, y), c) -> c == '1') $ build2dIndex disk

{-
  Transforms a list of lists into a list where every element in the original list of lists
  is associated with an index tuple specifying row and column.
-}
build2dIndex :: [[a]] -> [((Int, Int), a)]
build2dIndex list =
  concat $ map (\(rowIndex, row) -> zipWith (\row (col, a) -> ((row, col), a)) (repeat rowIndex) row) $ zip [0..] (map (zip [0..]) list)

{-
  Given the set of all coordinates to consider and a start point, find the largest
  contiguous block of adjacent coordinates. Coordinates are only adjacent and part of the contiguous block
  when they are next to, or to the top or bottom of, another coordinate. Diagonally, coordinates are not
  adjacent.
-}
findGroup :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
findGroup diskSet startPoint = findGroup' [startPoint] Set.empty
  where
    findGroup' :: [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
    findGroup' [] set = set
    findGroup' (x:xs) set
      | Set.member x set = findGroup' xs set
      | otherwise = findGroup' (xs ++ (neighbours x)) (Set.insert x set)
    neighbours :: (Int, Int) -> [(Int, Int)]
    neighbours (x, y) = filter ((flip Set.member) diskSet) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


-- Finds all contiguous blocks in the given set.
findAllGroups :: Set (Int, Int) -> [Set (Int, Int)]
findAllGroups diskSet = findAllGroups' (Set.toList diskSet) []
  where
    findAllGroups' :: [(Int, Int)] -> [Set (Int, Int)] -> [Set (Int, Int)]
    findAllGroups' [] sets = sets
    findAllGroups' (p:ps) sets
      | any (Set.member p) sets = findAllGroups' ps sets
      | otherwise = findAllGroups' ps ((findGroup diskSet p):sets)

testPart2Solve = findAllGroups (diskSet "flqrgnkx")

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ sum $ map (length . (filter (=='1'))) $ disk "wenycdww"
  putStrLn "\nPart 2:"
  putStrLn $ show $ length $ findAllGroups (diskSet "wenycdww")