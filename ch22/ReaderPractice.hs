module ReaderPractice where

import Control.Applicative
import Data.Monoid
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = pure (,) <*> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = pure (,) <*> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = pure (&&) <*> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = fmap summed $ pure (,) <*> xs <*> ys

main :: IO ()
main = do
  print . getAll . foldMap All . sequA $ 7
  print . sequA . fromMaybe 0 $ s'
  print . bolt . fromMaybe 0 $ ys
