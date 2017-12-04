import Data.List

passphrases :: String -> [[String]]
passphrases text = map words $ lines text

validPhrases :: ([String] -> Bool) -> [[String]] -> [[String]]
validPhrases check passphrases = filter check passphrases

isValidPassphrase :: [String] -> Bool
isValidPassphrase phrase = (length phrase) == (length $ nub phrase)

isValidPassphrase' :: [String] -> Bool
isValidPassphrase' phrase = (length unanagrammed) == (length $ nub unanagrammed)
  where
    unanagrammed = map sort phrase

solution = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ length $ validPhrases isValidPassphrase $ passphrases testInput
  putStrLn $ show $ length $ validPhrases isValidPassphrase $ passphrases input
  putStrLn "\nPart 2:"
  putStrLn $ show $ length $ validPhrases isValidPassphrase' $ passphrases testInput'
  putStrLn $ show $ length $ validPhrases isValidPassphrase' $ passphrases input
  
testInput = "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa"
testInput' = "abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj\niiii oiii ooii oooi oooo\noiii ioii iioi iiio"