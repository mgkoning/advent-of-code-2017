import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

take5 x = take 5 x [] where
  take _ [] ys = ys
  take 0 _ ys = ys
  take n (x:xs) ys = take (n-1) xs (x:ys)

data TimeParts = TimeParts { hours :: Integer, minutes :: Integer, seconds :: Integer }
instance Show TimeParts where
  show a = (show $ hours a) ++ " hours, " ++ (show $ minutes a) ++ " minutes, " ++ (show $ seconds a) ++ " seconds"

howLong = do
  now <- getCurrentTime
  let secondsLeft = floor $ timeLeft now
  if (secondsLeft < 0) then do
    putStrLn "It has started!"
  else do
    putStrLn $ (show $ timeParts secondsLeft) ++ " left."
  where
    timeLeft = diffUTCTime startTime
    startTime = zonedTimeToUTC $ ZonedTime (LocalTime dec1 midnight) est
    dec1 = fromGregorian 2017 12 1
    est = TimeZone (-5*60) False "EST"
    timeParts timeDiff = TimeParts
      {hours = quot timeDiff (60*60),
       minutes = rem (quot timeDiff 60) 60,
       seconds = rem timeDiff 60}


