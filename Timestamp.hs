module Timestamp 
( Timestamp(..)
, stringToTimestamp
) where
import System.Time

data Timestamp = Timestamp { year :: Int
                           , month :: Int
                           , day :: Int
                           , hour :: Int
                           , minute :: Int
                           , second :: Int
                           } deriving (Show)

parseTimestring :: String -> ClockTime
parseTimestring s = TOD sec 0
    where sec = read s

monthToInt :: Month -> Int
monthToInt m = getMonth m
    where months = zip [1..12] [January .. December]
          findMonth m = head $ filter (\(_, mon) -> mon == m) months
          getMonth = fst . findMonth

parseClockTime :: ClockTime -> Timestamp
parseClockTime ct = Timestamp tYear tMonth tDay tHour tMinute tSecond
    where calt = toUTCTime ct
          tYear = ctYear calt
          tMonth = monthToInt $ ctMonth calt
          tDay = ctDay calt
          tHour = ctHour calt
          tMinute = ctMin calt
          tSecond = ctSec calt

stringToTimestamp :: String -> Timestamp
stringToTimestamp = parseClockTime . parseTimestring