module Ch9Ex where

import Data.Char

splitString :: Char -> String -> [String]
splitString _ "" = []
--splitString " " = []
splitString splitter str = takeWhile (/=splitter) str : (splitString splitter (dropWhile (==splitter) ((dropWhile (/=splitter) str))))


firstLine = "Tyger, Tyger burning bright\n"
secondLine = "Like a meteor in the night\n"
thirdLine = "What immortal hand or eye,\n"
fourthLine = "Could frame thy fearful symmetry\n"

poem = firstLine ++ secondLine ++ thirdLine ++ fourthLine

myLines :: String -> [String]
myLines str = splitString ('\n') str

myWords :: String -> [String]
myWords str = splitString (' ') str


fb :: Int -> String
fb x
  | rem x 15 == 0 = "bop"
  | rem x 5  == 0 = "buzz"
  | rem x 3  == 0 = "fizz"
  | otherwise     = show x

--rmSmall :: String -> [String]
--rmSmall x = filter (isNotSmall) $ words x
--  where
--    isNotSmall str = not $ elem str ["the", "a","an"]
--

capFirst :: String -> String
capFirst "" = []
capFirst x = (toUpper ( head x) ): ( capFirst $ tail x)



