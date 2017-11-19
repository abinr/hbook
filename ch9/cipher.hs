module Cipher where

import Data.Char (ord, chr, isLower)

type Shift = Int
type PlainText = String
type CipherText = String

caesar :: Shift -> PlainText -> CipherText
caesar n =
  cipher (+n)

uncaesar n =
  cipher (subtract n)

cipher :: (Int -> Int) -> PlainText -> CipherText
cipher f = map encode
  where
    encode c =
      if isLower c
      then chr . (+) (ord 'a') . flip mod 26 . f $ ord c - ord 'a'
      else c



