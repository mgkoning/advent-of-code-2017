import Control.Applicative
import Data.List hiding (insert)
import Data.Ord
import Numeric
import Text.ParserCombinators.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Component = (Int, Int)
type BridgeInfo = ([Component], Int)

readComponents :: CharParser () [Component]
readComponents = sepBy readComponent (char '\n')

readComponent :: CharParser () Component
readComponent = (,) <$> (readPortSize <* char '/') <*> readPortSize

readPortSize :: CharParser () Int
readPortSize = do { n <- many1 digit; return (read n) }

unwrap p =
  case p of
    Left parseError -> error $ show parseError
    Right c -> c

makeComponentMap :: [Component] -> Map Int [Component]
makeComponentMap = foldl' addToMap Map.empty
  where
    addToMap map component = let (left, right) = component in insert component left (insert component right map)

insert component = Map.alter (addToList component)
  where
    addToList c = \v ->
      case v of
        Nothing -> Just [c]
        Just cs -> Just (c:cs)

remove component = Map.alter (removeFromList component)
  where
    removeFromList c = \v ->
      case v of
        Nothing -> error $ "Could not remove: component " ++ (show component) ++ " not present."
        Just cs -> Just (cs \\ [component])

bestBridge :: Map Int [Component] -> (BridgeInfo -> BridgeInfo -> Ordering) -> BridgeInfo
bestBridge components bestOrdering = let (bridge, strength) = bestBridge' components ([], 0) 0 in (reverse bridge, strength)
  where
    bestBridge' componentsLeft soFar lastPort =
      let (bridge, bridgeStrength) = soFar
          possibleConnections = Map.lookup lastPort componentsLeft
      in case possibleConnections of
        Nothing -> soFar
        Just [] -> soFar
        Just cs -> maximumBy bestOrdering $
          [bestBridge' comp (c:bridge, bridgeStrength + (strength c)) (other c lastPort)
            | c <- cs,
              let (left, right) = c,
              let comp = remove c left (remove c right componentsLeft)
          ]

strength (left, right) = left + right

other (left, right) used = if left == used then right else left

longestThenStrongest (b0, s0) (b1, s1) =
  let lengthCompare = compare (length b0) (length b1)
  in if lengthCompare == EQ then compare s0 s1 else lengthCompare

solve = do
  parseResult <- (parse readComponents "input file") <$> readFile "input.txt"
  let components = makeComponentMap $ unwrap parseResult
  putStrLn "Part 1:"
  putStrLn $ show $ snd $ bestBridge components (comparing snd)
  putStrLn "Part 2:"
  putStrLn $ show $ snd $ bestBridge components longestThenStrongest

testSolve = do
  let parseResult = parse readComponents "test input" testInput
  let components = makeComponentMap $ unwrap parseResult
  putStrLn $ show $ bestBridge components (comparing snd)
  putStrLn $ show $ bestBridge components longestThenStrongest

testInput = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"