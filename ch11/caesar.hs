module Caesar where

import Data.Char (ord, chr, isUpper)
import Data.Bool

type Shift = Int
type PlainText = String
type CipherText = String

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

caesar :: Shift -> PlainText -> CipherText
caesar n =
  map (cipher n)

decaesar :: Shift -> CipherText -> PlainText
decaesar n =
  map (cipher $ negate n)

cipher :: Int -> Char -> Char
cipher i c =
  if isUpper c
  then
    toScale26 c
    |> (+ i)
    |> flip mod 26
    |> fromScale26
  else c

toScale26 :: Char -> Int
toScale26 c =
  ord c - ord 'A'

fromScale26 :: Int -> Char
fromScale26 i =
  chr (ord 'A' + i)
