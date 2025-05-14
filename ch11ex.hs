module Ch11Ex where

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = 
   Mini
  | Mazda
  | Tata
  deriving (Eq,Show)

data Airline = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 69)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _          = False

areCars :: [Vehicle] -> Bool
areCars []     = True
areCars (x:xs) = (isCar x) && (areCars xs)

areCars' :: [Vehicle] -> Bool
areCars' = foldr (\v acc -> isCar v && acc) True

getMenu :: Vehicle -> Manufacturer
getMenu (Car m _) = m
getMenu (Plane _ _) = error "planes don't have manufacturer"


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany (n, str) = n > (length str)

instance TooMany (Int, Int) where
  tooMany (x,y) = (x + y) > 43

instance (Num a, Ord a, TooMany b) => TooMany (a,b) where
  tooMany (x, y) = (x > 53) || tooMany y


data BigSmall =
    Big Bool 
  | Small Bool
  deriving (Eq, Show)



data GuessWhat = 
  ChickenButt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First  a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving(Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)


data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse = 
  BigFarmHouse NumCow NumPig NumSheep

data BigFarmHouse' =
  BigFarmHouse' NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow   CowInfo
  | Pig   PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

type Awesome = Bool

type Person = Product Name Awesome
personA :: Person
personA = Product "Simon" True
personB = Product "Dingo" True

isSimon :: Person -> Bool
isSimon (Product "Simon" _) = True
isSimon (Product _ _) = False

data Twitter =
  Twitter deriving (Eq, Show)

data Askfm =
  Askfm deriving (Eq, Show)


data SocialNetwork' =
    Twitter'
  | Askfm'
  deriving (Eq, Show)

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)


data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allLanguages :: [ProgLang]
allLanguages = 
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allOs :: [OperatingSystem]
allOs =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

