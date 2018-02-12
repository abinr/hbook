module ApplTest where

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import List
import Ztanz

main :: IO ()
main = hspec $ do
  testBatch (applicative (ZipList $ Cons ("b", (1 :: Int), 'c') Nil))

instance Eq a => EqProp (List a) where
  xs =-= ys = take' 3000 xs `eq` take' 3000 ys

instance Eq a => EqProp (ZipList a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList l) = xs
                in take' 3000 l
          ys' = let (ZipList l) = ys
                in take' 3000 l

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [(1, pure Nil), (2, pure Cons <*> arbitrary <*> arbitrary)]

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = pure ZipList <*> arbitrary

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [ (1, pure List.Failure <*> arbitrary)
              , (2, pure List.Success <*> arbitrary)
              ]

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pure Pair <*> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = pure Two <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance
  (Arbitrary a
  , Arbitrary b
  , Arbitrary c)
  => Arbitrary (Three a b c) where
  arbitrary = pure Three <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = pure Three' <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance
  (Arbitrary a
  , Arbitrary b
  , Arbitrary c
  , Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = pure Four <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = pure Four' <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq
