import Data.Function
import Data.List
import Data.List.Split
import Data.Ord

data Vector = Vector {x :: Int, y :: Int, z :: Int} deriving (Show, Eq)
data Particle = Particle {number :: Int, pos :: Vector, vel :: Vector, acc :: Vector} deriving (Show)

readParticles = (zipWith (\n (p, v, a) -> Particle n p v a) [0..]) . (map readParticle) . lines

readParticle s =
  let (p:v:a:[]) = splitOn ", " s
      readVector t = let (x:y:z:[]) = map readSigned $ splitOn "," $ take (length t - 4) (drop 3 t)
                     in Vector x y z
  in (readVector p, readVector v, readVector a)

readSigned :: String -> Int
readSigned s
  | head s == '-' = negate (read $ tail s)
  | otherwise = read s

manhattanDistance v = abs (x v) + abs (y v) + abs (z v)

showDistances p = "Particle " ++ (show $ number p) ++
  ": p " ++ (show $ manhattanDistance $ pos p) ++
  " v " ++ (show $ manhattanDistance $ vel p) ++
  " a " ++ (show $ manhattanDistance $ acc p)

tick swarm = destroy $ map updateParticle swarm
  where destroy s = concat $ filter ((==1) . length) $ groupBy ((==) `on` pos) s

updateParticle p = let newVel = (vel p) <+> (acc p)
                   in p { pos = pos p <+> newVel, vel = newVel }

(<+>) :: Vector -> Vector -> Vector
(<+>) a b = Vector (x a + x b) (y a + y b) (z a + z b)

solve = do
  particles <- readParticles <$> readFile "input.txt"
  putStrLn "Part 1:"
  let top5Slowest = take 5 $ sortBy (comparing $ manhattanDistance . acc) particles
  putStrLn $ unlines $ map showDistances top5Slowest

  putStrLn "\nPart 2:"
  putStrLn $ show $ length $ last $ take 10000 $ iterate tick particles