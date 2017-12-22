import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

flipHor = map reverse

rotate = flipHor . transpose

start = lines ".#.\n..#\n###"

breakUp side = concat . map transpose . chunksOf side . map (chunksOf side)

stitch num = map concat . concat . map transpose . chunksOf num

readPatterns = Map.fromList . map readLine . lines
readLine = (\(lhs:rhs:[]) -> (splitOn "/" $ lhs, splitOn "/" $ rhs)) . (splitOn " => ")

findRule patterns input = head $ mapMaybe (`Map.lookup` patterns) $ [f . g | f <- [id, flipHor], g <- take 4 $ iterate (rotate .) id] <*> [input]

step patterns s = head $ mapMaybe 
  (\d -> let (quotD, remD) = length s `quotRem` d
         in if remD /= 0 then Nothing else Just (stitch quotD $ map (findRule patterns) (breakUp d s))
  ) [2, 3]

solve = do
  patterns <- readPatterns <$> readFile "input.txt"
  putStrLn "Part 1:"
  let steps = iterate (step patterns) start
  let part1 = head $ drop 5 $ steps
  showPat part1
  putStrLn $ show $ length $ concat  $ map (filter (=='#')) part1
  putStrLn "Part 2:"
  let part2 = head $ drop 18 $ steps
  --writeFile "part2-output.txt" (unlines part2)
  putStrLn $ show $ length $ concat  $ map (filter (=='#')) part2

showPat = do putStrLn . unlines

testSquare = "0123\n4567\n89ab\ncdef"
testSquare2 = "012345\n6789ab\ncdefgh\nijklmn\nopqrst\nuvwxyz"
testPatterns = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"