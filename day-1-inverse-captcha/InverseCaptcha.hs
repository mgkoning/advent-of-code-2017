import Data.Char

solveCaptcha xs = solveCaptcha' (mapToIntegers (appendHead xs)) 0
  where
    appendHead [] = []
    appendHead (x:xs) = [x] ++ xs ++ [x]
    mapToIntegers :: [Char] -> [Integer]
    mapToIntegers = map (toInteger . digitToInt)
    solveCaptcha' [] n = n
    solveCaptcha' (x:[]) n = n
    solveCaptcha' (x:y:xs) n = solveCaptcha' (y:xs) $ n + if x == y then x else 0

solveCaptcha2 list = solveCaptcha2' fullList 0 0
  where
    fullList = map digitToInt list
    captchaLength = length fullList
    offsetToLookAt = quot captchaLength 2
    solveCaptcha2' [] n _ = n
    solveCaptcha2' (x:xs) n index = solveCaptcha2' xs (n + addition) (index + 1)
      where addition = if x == fullList !! (rem (index + offsetToLookAt) captchaLength) then x else 0

