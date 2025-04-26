module Ch3ex where

ex1a :: String -> String
ex1a str = str

ex1b :: String -> String
ex1b str = [str !! 4]

ex1c :: String -> String
ex1c str = drop 9 str

--ex3
thirdLetter :: Int -> Char
thirdLetter idx = "Curry is Awesome" !! idx

rvrs :: String -> String
-- only works on "curry is awesome" to return "awesome is curry"
rvrs str = (drop 9 str) ++ (take 4 (drop 5 str)) ++ (take 5 str)

main :: IO ()
main = print $ rvrs "curry is awesome"
