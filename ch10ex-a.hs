module Ch10exA where

stops = "pbtdkg"
vowels = "aeiou"

allTuples = [(x,y,z) | x<-stops, y<-vowels, z<-stops, x == 'p']

seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc' x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))
