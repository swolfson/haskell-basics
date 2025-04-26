module Cipher where

import Data.Char

baseLowerChar = 'a'
baseLowerCharOrd = 97

baseUpperChar = 'A'
baseUpperCharOrd = ord 'A'

-- shift a char a certain number of spaces
shiftUpperChar :: Int -> Char -> Char
shiftUpperChar x c = chr $ (mod (x + ( ( ord c ) - baseUpperCharOrd )) 26) + baseUpperCharOrd

shiftLowerChar :: Int -> Char -> Char
shiftLowerChar x c = chr $ (mod (x + ( ( ord c ) - baseLowerCharOrd )) 26) + baseLowerCharOrd

ceasarString :: Int -> String -> String
ceasarString x cs = map (shift) cs
  where
    shift c
      | isLower c = shiftLowerChar x c
      | isUpper c = shiftUpperChar x c
