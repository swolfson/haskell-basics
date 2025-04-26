module RegisteredUser where

newtype Username = 
    Username String

newtype AccountNumber = 
    AccountNumber Integer

data User = 
    UnregisteredUser
    | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser = 
    putStrLn "Unregistered User"
printUser (RegisteredUser 
            (Username name)
            (AccountNumber acctNum) ) =
    putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
      Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = 
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

--gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
--gimmeWhereTheyLive (Peng whereItLives) = whereItLives

f' :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f' (a,b,c) (d,e,f) = ((a,d) , (c,f))


functionC x y =
  case z of
    True  -> x
    False -> y
  where z = (x > y)

ifEvenAdd2 n = 
    case even n of
      True -> n + 2
      False -> n

nums x = 
  case compare x 0 of
    LT -> (-1)
    GT -> 1
    _  -> 0

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = 
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             ->Employee
             -> Employee
             -> IO ()
employeeRank f e e' = 
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

dogYrs :: Float -> Float
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise        = False
   
