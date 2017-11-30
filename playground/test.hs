import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.LocalTime

take5 x = take 5 x [] where
  take _ [] ys = ys
  take 0 _ ys = ys
  take n (x:xs) ys = take (n-1) xs (x:ys)

howLong = do
  x <- left
  let secondsLeft = floor x
  let (hours, minutes, seconds) = (rem (quot secondsLeft (60*60)) 60, rem (quot secondsLeft 60) 60, rem secondsLeft 60)
  putStrLn ((show hours) ++ " hours, " ++ (show minutes) ++ " minutes, " ++ (show seconds) ++ " seconds left.")
  where
    now = getCurrentTime
    startTime = zonedTimeToUTC (ZonedTime (LocalTime (dec1) (midnight)) est)
    dec1 = fromGregorian 2017 12 1
    est = TimeZone (-5*60) False "EST"
    left = fmap (diffUTCTime startTime) now

