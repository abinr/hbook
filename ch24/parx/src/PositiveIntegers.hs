module PositiveIntegers where

import Text.Trifecta
import Control.Applicative
import qualified Data.Char as Char
import Data.Function

base10Integer :: Parser Integer
base10Integer =
  fmap digitsToInteger (some parseDigit)

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional $ char '-'
  num <- base10Integer
  case sign of
    Just '-' -> return $ negate num
    Nothing -> return num

digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . fmap (toInteger . Char.digitToInt)

parseDigit :: Parser Char
parseDigit = satisfyRange '0' '9' <?> "parseDigit"

listToInteger :: [Integer] -> Integer
listToInteger xs = foldl (\acc a -> acc * 10 + a) 0 xs
