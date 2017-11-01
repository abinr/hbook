{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

fiftyfour = (*9) 6

firstPair = head [(0, "doge"), (1, "kitten")]

firstPair' = head [(0 :: Integer, "doge"), (1, "kitten")]

decide = if False then True else False

len = length [1,2,3,4,5]

gara = (length [1..4]) > (length "TACOCAT")

x = 5
y = x + 5
w = y * 10
z y = y * 10
f = 4 / y

a = "Julie"
b = "<3"
c = "Haskell"
g = a ++ b ++ c

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x < y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y
