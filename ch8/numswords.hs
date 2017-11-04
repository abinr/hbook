module WordNumber where

import Data.List

-- careful only handles 0-9
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

-- careful only handles natural numbers
digits :: Int -> [Int]
digits n = go n []
  where
    go 0 xs = xs
    go n xs =
      go (n `div` 10) ([n `mod` 10] ++ xs)

-- careful only handles natural numbers
wordNumber :: Int -> String
wordNumber =
  concat . intersperse "-" . map digitToWord . digits




