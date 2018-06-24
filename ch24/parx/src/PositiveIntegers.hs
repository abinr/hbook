module PositiveIntegers where

import Text.Trifecta
import qualified Data.Char as Char

parseDigit :: Parser Char
parseDigit = satisfyRange '0' '9' <?> "parseDigit"

base10Integer :: Parser Integer
base10Integer =
  fmap listToInteger $ (fmap . fmap) (toInteger . Char.digitToInt) ints

ints :: Parser String
ints =  pure (:) <*> parseDigit <*> many parseDigit

listToInteger :: [Integer] -> Integer
listToInteger xs = foldl (\acc a -> acc * 10 + a) 0 xs
