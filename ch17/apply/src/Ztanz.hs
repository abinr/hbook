module Ztanz where

import Data.Monoid ((<>))

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two u f) (Two v x) = Two (u <> v) (f x)

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three x y z) (Three x' y' z') =
    Three (x <> x') (y <> y') (z <> z')

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three u v f) (Three u' v' x) =
    Three (u <> u') (v <> v') (f x)

data Three' a b
  = Three' a b b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' x y z) (Three' x' y' z') =
    Three' (x <> x') (y <> y') (z <> z')

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f g) (Three' y u v) =
    Three' (x <> y) (f u) (g v)

data Four a b c d
  = Four a b c d
  deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend (Four w x y z) (Four w' x' y' z') =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four w x y f) (Four w' x' y' z) =
    Four (w <> w') (x <> x') (y <> y') (f z)

data Four' a b
  = Four' a b b b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend (Four' w x y z) (Four' w' x' y' z') =
    Four' (w <> w') (x <> x') (y <> y') (z <> z')

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w (f x) (f y) (f z)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty x x x
  (<*>) (Four' a f g h) (Four' a' x y z) =
    Four' (a <> a') (f x) (g y) (h z)
