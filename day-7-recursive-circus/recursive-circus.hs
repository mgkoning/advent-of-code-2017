import Data.List

readInput :: String -> [(String, Int, [String])]
readInput input = [(name, weight, if null supported then supported else tail supported) |
    line <- wordsByLine,
    let name = head line,
    let weight = read $ takeWhile (/=')') $ dropWhile (=='(') $ head $ tail line,
    let supported = map (takeWhile (/=',')) $ dropWhile (/="->") line]
  where
    wordsByLine = map words $ lines input

solution = do
  inputString <- readFile "input.txt"
  let input = readInput inputString
  --putStrLn $ show $ input
  let allPrograms = map (\(x, _, _) -> x) input
  let supportedPrograms = foldl1 (++) $ map (\(_, _, z) -> z) input
  putStrLn "Part 1:"
  let root = allPrograms \\ supportedPrograms
  putStrLn $ show root