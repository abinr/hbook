module Warmup where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = pure (,) <*> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  c <- cap
  r <- rev
  return (r, c)

-- tupled'' :: [Char] -> ([Char], [Char])
tupled'' =
  cap >>= \c -> rev >>= \r -> return (r, c)

