{-# LANGUAGE DeriveGeneric #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Generics
import Data.List (sort)
import Data.Char

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Integral a => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Integral a => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

minusAssociative :: Integral a => a -> a -> a -> Bool
minusAssociative x y z =
  x - (y - z) == (x - y) - z

multAssoc :: Integral a => a -> a -> a -> Bool
multAssoc x y z =
  x * (y * z) == (x * y) * z

multComm :: Integral a => a -> a -> Bool
multComm x y =
  x * y == y * x

quotRemProp :: Integral a => a -> NonZero a -> Bool
quotRemProp x (NonZero y) =
  x == (quot x y) * y + (rem x y)

divModProp :: Integral a => a -> NonZero a -> Bool
divModProp x (NonZero y) =
  x == (div x y) * y + (mod x y)


data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

data Pair a b =
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (First a), return (Second b)]

data Bool'
 = True'
 | False'
 deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

instance Arbitrary Fool where
  arbitrary = foolGen

foolGen' :: Gen Fool
foolGen' = do
  frequency [(2, return Fulse), (1, return Frue)]

main :: IO ()
main = do
  sample trivialGen
  sample (identityGen :: Gen (Identity Char))
  sample (pairGen :: Gen (Pair Int String))
  sample (sumGen :: Gen (Sum Char Bool))
  sample (trueGen)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = (toUpper x) : xs

f x =
  (capitalizeWord x == twice capitalizeWord x)
  && (capitalizeWord x == fourTimes capitalizeWord x)

f' x =
  (sort x == twice sort x)
  && (sort x == fourTimes sort x)
