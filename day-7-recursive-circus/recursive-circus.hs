import Data.List
import qualified Data.Map.Strict as Map

data Disc = Disc {name :: String, weight :: Int, supported :: [Disc]}

readInput :: String -> [(String, Int, [String])]
readInput input = [(name, weight, supported) |
    line <- wordsByLine,
    let name = head line,
    let weight = read $ takeWhile (/=')') $ dropWhile (=='(') $ head $ tail line,
    let supported = map (takeWhile (/=',')) $ dropWhile (=="->") $ dropWhile (/="->") line]
  where
    wordsByLine = map words $ lines input

buildTree :: [(String, Int, [String])] -> Disc
buildTree s = Disc "not done" 0 []
  where
    programsMap = Map.fromList (map (\(n,w,s) -> (n, (n, w, s))) s)
    discMap = Map.empty :: Map.Map String [Disc]
  

solution = do
  inputString <- readFile "input.txt"
  let input = readInput inputString
  let allPrograms = map (\(x, _, _) -> x) input
  let supportedPrograms = foldl1 (++) $ map (\(_, _, z) -> z) input
  putStrLn "Part 1:"
  let root = allPrograms \\ supportedPrograms
  putStrLn $ show root
