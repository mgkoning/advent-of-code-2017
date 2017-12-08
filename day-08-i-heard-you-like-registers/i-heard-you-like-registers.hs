import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Foldable

data Instruction = Instruction {
  opRegister :: String,
  operation :: (Int -> Int),
  guardRegister :: String,
  guard :: (Int -> Bool)
}

readInstructions :: String -> [Instruction]
readInstructions s = map (parse . words) $ lines s
  where
    parse (a:oper:operArg:"if":b:guard:guardArg:[]) = Instruction a (makeOperation oper (parseInt operArg)) b (makeGuard guard (parseInt guardArg))
    parse _ = error "bad line"

makeOperation :: String -> Int -> (Int -> Int)
makeOperation o arg =
  case o of
    "dec" -> (subtract arg)
    "inc" -> (+arg)

makeGuard :: String -> Int -> (Int -> Bool)
makeGuard g arg =
  case g of
    "==" -> (==arg)
    "!=" -> (/=arg)
    "<" -> (<arg)
    ">" -> (>arg)
    "<=" -> (<=arg)
    ">=" -> (>=arg)

parseInt :: String -> Int
parseInt s
  | head s == '-' = negate (read (tail s))
  | otherwise = read s

calculate :: [Instruction] -> (Map.Map String Int, Int)
calculate instructions = foldl' update (Map.empty, 0) instructions
  where
    update :: (Map.Map String Int, Int) -> Instruction -> (Map.Map String Int, Int)
    update (state, previousMax) instr = (newState, maximum (previousMax:(Map.elems newState)))
      where
        newState = Map.alter updateEntry (opRegister instr) state
        updateEntry  :: Maybe Int -> Maybe Int
        updateEntry previous
          | guard instr (fromMaybe 0 (Map.lookup (guardRegister instr) state)) = Just (operation instr (fromMaybe 0 previous))
          | otherwise = previous

solve = do
  putStrLn "Test input: "
  putStrLn $ show $ calculate $ readInstructions $ testInput

  input <- readFile "input.txt"
  let instructions = readInstructions input
  let (result, maxValue) = calculate instructions
  putStrLn "\nPart 1:"
  putStrLn $ show $ maximum $ Map.elems result
  putStrLn "\nPart 2:"
  putStrLn $ show maxValue


testInput = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"