module Main where

import Data.Char (ord, chr, isUpper)
import Data.Bool
import Test.QuickCheck

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

assign :: Int -> String -> String -> [(Char, Char)]
assign i [] _ = []
assign i (x:xs) salt =
  if isUpper x
  then (encode i salt, x) : assign (i + 1) xs salt
  else (x, x) : assign i xs salt

encode i salt =
  salt !! mod i (length salt)

vigenere secret salt =
  assign 0 secret salt
  |> map (\(x, y) -> (toScale26 x, [y]))
  |> map (uncurry caesar)
  |> concat

devigenere cipher salt =
  assign 0 cipher salt
  |> map (\(x, y) -> (toScale26 x, [y]))
  |> map (uncurry decaesar)
  |> concat

validInputGen :: Gen String
validInputGen =
  listOf1 $ elements $ ['A'..'Z'] ++ [' ']

type Shift = Int
type PlainText = String
type CipherText = String

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

main :: IO ()
main = do
  putStrLn "Enter plain text: "
  t <- getLine
  putStrLn "Enter salt: "
  s <- getLine
  let r = vigenere t s
  putStrLn $ "Cipher text: " ++ r
