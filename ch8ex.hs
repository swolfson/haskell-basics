module Ch8Ex where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = error "invalid number"

digits' :: Int -> [Int]
digits' 0 = [0]
digits' x = map (read . (:[])) (show x)

digits :: Int -> [Int]
digits x = tail (reverse (tmpDigs x))
  where 
  tmpDigs y  
    | rem == 0 = [y]
    | otherwise = rem : tmpDigs q
    where
      q = (div) y 10
      rem = (mod) y 10

wordNumber :: Int -> String
wordNumber x = concat (intersperse "-" (digitWords x))
  where
  digitWords  = map digitToWord . digits'
