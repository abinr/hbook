import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Data.Monoid

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure Identity <*> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

newtype Constant a b
  = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = pure Constant <*> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = fmap Yep (f x)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (3, pure Yep <*> arbitrary)]

instance Eq a => EqProp (Optional a) where (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, pure Cons <*> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where (=-=) = eq

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = fmap (Three x y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = pure Three <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = fmap (Pair x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pure Pair <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ y z) = f y <> f z
  
instance Traversable (Big a) where
  traverse f (Big x y z) = pure (Big x) <*> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = pure Big <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = pure (Bigger a) <*> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
   arbitrary = pure Bigger <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

data S n a
  = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S s x) = S (fmap f s) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S s x) = foldMap f s <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S s x) = pure S <*> traverse f s <*> f x

instance (Arbitrary (n a), Arbitrary a, CoArbitrary a) => Arbitrary (S n a) where
  arbitrary = pure S <*> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = pure Leaf <*> f x
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [ (1, pure Empty)
              , (2, pure Leaf <*> arbitrary)
              , (3, pure Node <*> arbitrary <*> arbitrary <*> arbitrary)
              ]

instance Eq a => EqProp (Tree a) where (=-=) = eq 
