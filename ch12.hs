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

newtype Word' =
  Word' String
  deriving (Eq, Show)


vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = case numVow > numCons of
  True -> Nothing
  False -> Just $ Word' str
  where
    numVow = countVowels str
    numCons = (length str) - numVow

data Nat = 
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer ->  Maybe Nat
integerToNat n 
  | n < 0  = Nothing
  | n == 0 = Just Zero
  | n > 0  = Just (go n)
  where 
    go 0 = Zero
    go n = Succ (go (n-1))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _)  = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _)  = False

mayybee :: b -> (a->b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x f (Just y) = f y 

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x id y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = (maybeToList x) ++ (catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe (Just x: xs) = case flipMaybe xs of
  Nothing -> Nothing
  Just ys -> Just (x:ys)

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) acc = x : acc
    f (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) acc = x : acc
    f (Left _) acc = acc

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' = foldr f ([],[])
  where
    f (Left x) (accL, accR)  = ( (x:accL), accR)
    f (Right x) (accL, accR) = ( accL, (x:accR))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left x) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb (Left x) = fa x
either' fa fb (Right x) = fb x

eitherMaybe'' :: (b->c) -> Either a b -> Maybe c
eitherMaybe'' fb ex = either' fa fb' ex
  where
    fa = \_ -> Nothing
    fb' = Just . fb

myIterate :: (a->a) -> a -> [a]
myIterate f x =  [x] ++ (myIterate f (f x))

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr fb xb = xa' ++ (myUnfoldr fb xb')
  where
    xa' = case (fb xb) of
      Just (xa'', _) -> [xa'']
      Nothing -> []
    xb' = case (fb xb) of
      Just (_, xb'') -> xb''
      Nothing        -> xb
