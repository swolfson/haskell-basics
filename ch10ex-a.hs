module Ch10exA where


stops = "pbtdkg"
vowels = "aeiou"

allTuples = [(x,y,z) | x<-stops, y<-vowels, z<-stops, x == 'p']

seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc' x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))


myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False 

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc -> (==x) y || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x: acc) []

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a->[b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a->a->Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\acc x -> case f acc x of
  LT -> x
  _ -> acc 
  ) x xs
myMaximumBy _ [] = error "empty list"

myMinimumBy :: (a->a->Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\acc x -> case f acc x of
  LT -> acc
  _ -> x 
  ) x xs
myMinimumBy _ [] = error "empty list"
