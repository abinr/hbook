import Control.Monad
import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers

bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = join $ fmap f mx


data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Monoid a => Applicative (Sum a) where
  pure = Second
  (<*>) (First x) (First x') = First (x <> x')
  (<*>) _ (First x) = First x
  (<*>) (First x) _ = First x
  (<*>) (Second f) (Second x) = Second (f x)

instance Monoid a => Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [pure First <*> arbitrary, pure Second <*> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

data PhhhbbtttEither b a
  = Lyft a
  | Roght b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Lyft x) = Lyft (f x)
  fmap _ (Roght b) = Roght b

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure = Lyft
  (<*>) (Roght b) (Roght b') = Roght (b <> b')
  (<*>) (Roght b) _ = Roght b
  (<*>) _ (Roght b) = Roght b
  (<*>) (Lyft f) (Lyft x) = Lyft (f x)

instance Monoid b => Monad (PhhhbbtttEither b) where
  (Roght x) >>= _ = Roght x
  (Lyft x) >>= f = f x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [pure Lyft <*> arbitrary, pure Roght <*> arbitrary]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure Identity <*> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

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
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) (Cons x xs) =
    Cons (f x) (fmap f xs) <> (fs <*> Cons x xs)

instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, pure Cons <*> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where (=-=) = eq

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = fmap f x >>= \g -> fmap g y

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf >>= \f -> fmap f ma

meh :: Monad m => [a] -> (a -> m b)-> m [b]
meh xs f = turnInsideOut $ fmap f xs

turnInsideOut :: Monad m => [m b] -> m [b]
turnInsideOut [] = pure []
turnInsideOut (x:xs) = pure (:) <*> x <*> turnInsideOut xs

flipType :: Monad m => [m a] -> m [a]
flipType = turnInsideOut
