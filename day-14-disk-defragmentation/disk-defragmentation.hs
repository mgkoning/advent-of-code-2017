import Data.Map.Strict hiding (map, filter)
import KnotHash

disk :: String -> [String]
disk key = map (knotHashToBinString . ((key ++ "-") ++) . show) [0..127]

diskMap :: String -> Map (Int, Int) Char
diskMap key = fromList $ concat $ map (\(ri, row) -> zipWith (\x (y, c) -> ((x, y), c)) (repeat ri) row) $ zip [0..] (map (zip [0..]) (disk key))

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ sum $ map (length . (filter (=='1'))) $ disk "wenycdww"
  putStrLn "\nPart 2:"