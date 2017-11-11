fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs' :: [Integer]
fibs' =
  take 20 fibs

fibs'' :: [Integer]
fibs'' = takeWhile (<100) fibs

{-- infinite list? --}
factorial :: Integer -> Integer
factorial n =
  last $ scanl (*) 1 [1..n]
