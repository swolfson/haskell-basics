module Ch8A where

myFactorial :: Integer -> Integer
myFactorial 0 = 1
myFactorial n = n * myFactorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes (-1) _ = 0
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times -1) n)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (fibonacci (x - 1)) + (fibonacci (x-2))

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where 
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)
