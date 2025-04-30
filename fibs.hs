module Fibs where

fibs = takeWhile (<100) $ 1 : scanl (+) 1 fibs 
fibsN x = fibs !! x

facts x = scanl (*) 1 [1..x]

factsN x = facts x !! x
