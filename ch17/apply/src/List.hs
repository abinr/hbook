module List where

import Control.Applicative hiding (ZipList)
import Data.Monoid ((<>))

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 ls = ls
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x xs) (Cons y ys) = Cons x (xs `mappend` (Cons y ys))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> (Cons x xs) =
    Cons (f x) (fmap f xs) `mappend` (fs <*> Cons x xs)

newtype ZipList a
  = ZipList (List a)
  deriving (Eq, Show)

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
  pure = ZipList . pure
  (<*>) (ZipList Nil) _ = ZipList Nil
  (<*>) _ (ZipList Nil) = ZipList Nil
  (<*>) (ZipList (Cons f Nil)) (ZipList (Cons x xs)) =
    ZipList $ Cons (f x) (pure f <*> xs)
  (<*>) (ZipList (Cons f fs)) (ZipList (Cons x Nil)) =
    ZipList $ Cons (f x) (fs <*> pure x)
  (<*>) (ZipList (Cons f fs)) (ZipList (Cons x xs)) =
    ZipList $ Cons (f x) (fs <*> xs)

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure x <*> Failure y = Failure (x <> y)
  Failure x <*> _ = Failure x
  _ <*> Failure x = Failure x
  Success f <*> Success x = Success (f x)
