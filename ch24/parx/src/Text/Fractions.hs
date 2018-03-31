{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String(IsString)
import Text.Trifecta

main :: IO ()
main = do
  putStrLn "=== ATTOP"
  let attoP = parseOnly parseDecimalOrFraction
  print $ attoP alsoBad
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP "0.0" -- shouldWork
  print $ attoP "3.14" -- shouildWork
  print $ attoP ".03" -- bad decisal

  putStrLn "\n=== TRIFECTA"
  let p i = parseString parseDecimalOrFraction mempty i
  print $ p alsoBad
  print $ p badFraction
  print $ p shouldWork
  print $ p shouldAlsoWork
  print $ p "0.0" -- shouldWork
  print $ p "3.14" -- shouildWork
  print $ p ".03" -- bad decisal

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: (Monad m, TokenParsing m) => m Rational
parseDecimal = fmap toRational double

parseDecimalOrFraction :: (Monad m, TokenParsing m) => m Rational
parseDecimalOrFraction = try parseFraction <|> parseDecimal
