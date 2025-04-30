module Ch10Ex where

import Data.Time
import Data.List (foldr)

data DatabaseItem = DbString String 
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9002
  , DbString "Hello, World!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr extractDate [] items
  where
    extractDate (DbDate time) acc = time : acc
    extractDate _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = foldr extractNumber [] items
  where
    extractNumber (DbNumber num) acc = num : acc
    extractNumber _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = maximum dates
  where
    dates = filterDbDate items

sumDb :: [DatabaseItem] -> Integer
sumDb items = sum numbers
  where
    numbers = filterDbNumber items

avgDb :: [DatabaseItem] -> Double
avgDb items = total / count
  where 
    total = fromIntegral $ sumDb items
    count = fromIntegral $ length (filterDbNumber items)


      
