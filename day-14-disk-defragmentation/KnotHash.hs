module KnotHash (knotHash, knotHashToHexString, knotHashToBinString) where 

  import Data.Bits
  import Data.Char
  import Data.List
  import Data.List.Split
  import Numeric
  
  
  -- Makes a list of lengths into a list of lengths with skip sizes.
  transformations :: [Int] -> [(Int, Int)]
  transformations input = zip input [0..]

  {-
    A single knot application. Rather than keeping track of position, the input is shifted at the end.
    That means that every application of 'tieKnot' can assume to start at position 0.
  -}
  tieKnot :: [Int] -> (Int, Int) -> [Int]
  tieKnot state (stepLength, skipSize) =
    let
      transform = (reverse $ take stepLength state) ++ drop stepLength state
    in rotateLeft (stepLength + skipSize) transform

  {-
    Rotates the content of the list left by n positions. Makes use of Haskell's ability to have
    infinite lists by applying 'cycle' to input.
  -}
  rotateLeft :: Int -> [Int] -> [Int]
  rotateLeft n xs = take (length xs) $ drop n (cycle xs)

  {-
    Performs the knot hash calculations. Output from the fold will be shifted to the left
    many times. This is remedied by calculating the effective left shift and then shifting it
    in the opposite direction. Note that rotateRight n xs = rotateLeft (length xs - n) xs.
  -}
  knotHash :: [Int] -> [Int]
  knotHash input = result
    where
      startState = [0..255]
      transformResult = foldl' tieKnot startState (transformations input)
      effectiveShift = (sum $ zipWith (+) input [0..]) `rem` length startState
      result = rotateLeft (length startState - effectiveShift) transformResult

  {-
    Sets up the input to perform the hash from part 2. Performing 64 rounds just means
    repeating the input (appended with fixed bytes) 64 times before calling knotHash.
    The result is XOR-ed in chunks of 16 bytes.
  -}
  knotHashPart2 :: [Int] -> [Int]
  knotHashPart2 input =
    let hashInput = input ++ [17, 31, 73, 47, 23]
        hash = knotHash (take (64 * length hashInput) (cycle hashInput))
        denseHash = map (foldl1' xor) (chunksOf 16 hash)
    in denseHash

  -- Hex representation of hash.
  showHashHex :: [Int] -> String
  showHashHex = concat . (map (reverse . (take 2) . reverse . ('0':) . (`showHex` "")))

  -- Helper function to do allow calculating straight from string.
  knotHashToHexString :: String -> String
  knotHashToHexString = showHashHex . knotHashPart2 . (map ord)

  showBin :: Int -> ShowS
  showBin a = showIntAtBase 2 intToDigit a

  -- Binary representation of hash.
  showHashBin :: [Int] -> String
  showHashBin = concat . (map (reverse . (take 8) . reverse . ("0000000"++) . (`showBin` "")))

  -- Helper function to do allow calculating straight from string.
  knotHashToBinString :: String -> String
  knotHashToBinString = showHashBin . knotHashPart2 . (map ord)