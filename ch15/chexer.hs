{-# LANGUAGE DeriveGeneric #-}
import Data.Semigroup
import Test.QuickCheck
import GHC.Generics

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

----------------

data Trivial = Trivial deriving (Eq, Show)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = pure Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-----------------

newtype Identity a = Identity a deriving (Eq, Show)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity ( x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

------------------

data Two a b = Two a b deriving (Eq, Show)

type TwoAssoc
  = Two String [Int]
  -> Two String [Int]
  -> Two String [Int]
  -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two (mempty) (mempty)
  mappend = (<>)

------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three String String String ->
                  Three String String String ->
                  Three String String String ->
                  Bool

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc = Four String String String String ->
                 Four String String String String ->
                 Four String String String String ->
                 Bool

instance
  (Semigroup a
  , Semigroup b
  , Semigroup c
  , Semigroup d) =>
  Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj False <> BoolConj _ = BoolConj False
  BoolConj _ <> BoolConj False = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

instance Semigroup BoolDisj where
  BoolDisj True <> BoolDisj _ = BoolDisj True
  BoolDisj _ <> BoolDisj True = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, fmap Fst arbitrary), (1, fmap Snd arbitrary)]

instance Semigroup (Or a b) where
  Fst a <> Fst b = Fst b
  Fst a <> Snd b = Snd b
  Snd a <> Snd b = Snd b
  Snd a <> Fst b = Snd a

{-- I have no idea how to do this
newtype Combine a b =
  Combine { unCombine :: (a -> b) }
  deriving (Generic)

instance Show (Combine a b) where
  show (Combine _) = "Combine function...which I cannot show"

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Combine a b)

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

combineGen :: Gen Int
combineGen = coarbitrary unCombine arbitrary

type CombineAssoc =
  Combine Int String -> Combine Int String -> Combine Int String -> Bool
--}

data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = 
      frequency [(1, fmap Failure' arbitrary), (1, fmap Success' arbitrary)]

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (Failure' x) <> (Failure' y) = Failure' (x <> y)
  (Success' x) <> (Failure' y) = Success' x
  (Failure' x) <> (Success' y) = Success' y
  (Success' x) <> (Success' y) = Success' x

type ValidationAssoc = Validation String String
                     -> Validation String String
                     -> Validation String String
                     -> Bool

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem x) (Mem y) = Mem $ \s ->
    let
      (a, s') = x s
      (a', s'') = y s'
    in
      (a `mappend` a', s'')

f :: Mem Integer String
f = Mem $ \s -> ("hi", s + 1)

testMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f `mappend` mempty) 0
      rmright = runMem (mempty `mappend` f) 0
  print $ rmleft
  print $ rmright
  print (rmzero :: (String, Integer))
  print $ rmleft == runMem f 0
  print $ rmright == runMem f 0
