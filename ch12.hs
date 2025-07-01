module Ch12 where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge)   = Left (badName ++ badAge)
mkPerson' (Left badName) _   = Left badName 
mkPerson' _ (Left badAge)    = Left badAge 

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe cs = Just cs

replaceThe :: String -> String
replaceThe ""   = ""
replaceThe strs = unwords $ aToThe $ words strs
  where
    aToThe :: [String] -> [String]
    aToThe (w:[]) = case notThe w of
      Nothing -> ["a"]
      Just w -> [w]
    aToThe (w:ws) = case notThe w of
      Nothing -> ["a"] ++ aToThe ws
      Just w  -> w : aToThe ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel text = countPairs (words text)
  where
    countPairs [] = 0
    countPairs [_] = 0
    countPairs (w1:w2:ws) = 
      if w1 == "the" && startsWithVowel w2
      then  1 + countPairs (w2:ws)
      else countPairs (w2:ws)
    startsWithVowel [] = False
    startsWithVowel (c:_) = c `elem` "aeiouAEIOU"

countVowels :: String -> Int
countVowels "" = 0
countVowels cs =  length (strVwls cs)
  where
    strVwls [] = ""
    strVwls (c:cs) = case (c `elem` "aeiouAEIOU") of
      True -> c : (strVwls cs)
      False -> strVwls cs

