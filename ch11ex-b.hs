module Ch11exB where
import Data.Char 
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf s1@(c1:[]) s2@(c2:[]) = c1 == c2
isSubseqOf s1@(c1:_) s2@(c2:[]) = c1 == c2
isSubseqOf s1@(c1:[]) s2@(c2:cs2) = c1 == c2 || isSubseqOf s1 cs2
isSubseqOf s1@(c1:cs1) s2@(c2:cs2) = ((c1 == c2) && isSubseqOf cs1 cs2) || (isSubseqOf s1 cs2)

subTest1 = (isSubseqOf "blah" "woot") == False
subTest2 = (isSubseqOf "blah" "wootblah") == True
subTest3 = (isSubseqOf "blah" "blahwoot") == True
subTest4 = (isSubseqOf "blah" "wboloath") == True
subTest5 = (isSubseqOf "blah" "wboloaht") == True
subTest6 = (isSubseqOf "blah" "wbolhaot") == False

subTests = subTest1 && subTest2 && subTest3 && subTest4 && subTest5 && subTest6


capitalizeWords :: String -> [(String,String)]
capitalizeWords s = map (\w -> (w, capitalize w)) (words s)
  where
    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':cs) = ' ':(capitalizeWord cs)
capitalizeWord (c:cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph para = 
  let sentences = splitOn '.' para
      capSentences = map capitalizeWord sentences
  in intercalate "." capSentences
  where
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn c str =
      case break ( == c) str of
        (part, []) -> [part]
        (part, _:rest) -> part : splitOn c rest

    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs


