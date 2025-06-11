module VCipher where

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

findShift :: Char -> Int
findShift k = (ord k) - base
  where 
    base
      | isLower   k = baseLowerCharOrd
      | otherwise  = baseUpperCharOrd 


vigenereString :: String -> String -> String
vigenereString key message = processMessage message (cycle key)
  where
    processMessage [] _ = []
    processMessage (c:cs) keyStream
      | isAlpha c = shift c (head keyStream) : processMessage cs (tail keyStream)
      | otherwise = c : processMessage cs keyStream  -- don't advance key
    
    shift c keyChar
      | isLower c = shiftLowerChar (findShift keyChar) c
      | isUpper c = shiftUpperChar (findShift keyChar) c


doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


