module Ch9Ex where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = myAny ((==)x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a->[b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs 

myMaximumBy :: (a->a->Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy fcomp (x:xs) = 
  case n of
    LT -> y
    _ -> x
  where
    y = myMaximumBy fcomp xs
    n = fcomp x y

myMinimumBy :: (a->a->Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy fcomp (x:xs) = 
  case n of
    GT -> y
    _ -> x
  where
    y = myMinimumBy fcomp xs
    n = fcomp x y

myMaximum :: (Ord a) => [a] ->a
myMaximum x = myMaximumBy compare x

myMinimum:: (Ord a) => [a] ->a
myMinimum x = myMinimumBy compare x
