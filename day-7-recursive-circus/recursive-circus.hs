import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
import Data.Function

data Disc = Disc {
  name :: String, weight :: Int, totalWeight :: Int, supported :: [Disc]
} deriving Show

{-
  Reads the input by line. Lines are divided into whitespace separated 'tokens'.
  Unnecessary delimiters and such are dropped from the tokens.
  Assumes that input is valid.
-}
readInput :: String -> [(String, Int, [String])]
readInput input = [(name, weight, supported) |
    line <- wordsByLine,
    let name = head line,
    let weight = read $ takeWhile (/=')') $ dropWhile (=='(') $ head $ tail line,
    let supported = map (takeWhile (/=',')) $ drop 1 $ dropWhile (/="->") line]
  where
    wordsByLine = map words $ lines input

{-
  Build a 'Disc' structure (a tree) using the parsed list. For ease of use, the total weight, i.e.,
  the sum of the node's weight and all its children's weights, is also calculated here.
-}
buildTree :: String -> [(String, Int, [String])] -> Disc
buildTree root s = makeDisc root
  where
    -- A map is used for faster lookup.
    programsMap = Map.fromList (map (\(n,w,s) -> (n, (n, w, s))) s)
    makeDisc :: String -> Disc
    makeDisc name = Disc n w (w + sum (map totalWeight children)) children
      where 
        (n, w, s) = programsMap Map.! name
        children = [child | childName <- s, let child = makeDisc childName]

{-
  Find the unbalanced node. If all children have the same weight, the unbalance started
  at the input Disc. Otherwise, one of the children must be unbalanced compared to the others:
  the 'bad child'. The weight of the other children is passed into the recursion, so we can
  determine what its weight should have been if the 'bad child' was the cause of the unbalance.
-}
findUnbalanced :: Int -> Disc -> (Disc, Int)
findUnbalanced requiredWeight disc
  | length (nub $ map totalWeight children) == 1 = (disc, requiredWeight)
  | otherwise = findUnbalanced requiredChildWeight badChild
    where
      children = supported disc
      badChild = head $ head childrenByWeight
      requiredChildWeight = totalWeight $ head $ head $ drop 1 $ childrenByWeight
      childrenByWeight = sortBy (comparing length) $ groupBy ((==) `on` totalWeight) $ sortBy (comparing totalWeight) children

solution = do
  inputString <- readFile "input.txt"
  let input = readInput inputString
  let allPrograms = map (\(x, _, _) -> x) input
  let supportedPrograms = foldl1 (++) $ map (\(_, _, z) -> z) input

  putStrLn "Part 1:"
  let root = head $ allPrograms \\ supportedPrograms
  putStrLn $ root
  
  putStrLn "\nPart 2 test:"
  let testTree = buildTree "tknk" (readInput testInput)
  putStrLn $ show $ findUnbalanced 0 testTree

  putStrLn "\nPart2:"
  let (unbalanced, requiredWeight) = findUnbalanced 0 $ buildTree root input
  putStrLn $ "Unbalanced node: " ++ (name unbalanced) ++ ", weight: " ++ (show $ weight unbalanced) ++ ", total weight: " ++ (show $ totalWeight unbalanced)
  putStrLn $ "Required total weight: " ++ (show requiredWeight)
  putStrLn $ "Needed weight: " ++ (show $ (weight unbalanced) + (requiredWeight - (totalWeight unbalanced)))


testInput = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\n" ++
  "fwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\n" ++
  "tknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"