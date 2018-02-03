{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import GHC.Arr

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++"lol") (Just ["Hi,", "Hello"])

c = (*2) . (\x -> x - 2)

d = ((return '1' ++) . show) . (\x -> [x, 1..3])

{--
e :: IO Integer
e =
  let
    ioi = readIO "1" :: IO Integer
    changed = (fmap . fmap) (:[]) . fmap (("123"++) . show) $ ioi
    r = (fmap . fmap) ((*3) . read) changed
  in
    fmap (read . concat) $ (fmap . fmap) show r
--}

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

e :: IO Integer
e =
  (readIO "1" :: IO Integer)
  |> fmap (:[]) -- make singleton [Integer]
  |> fmap ([1..3] ++)
  |> (fmap . fmap) (*3)
  |> (fmap. fmap) show
  |> fmap concat
  |> fmap read

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
               -> (b -> c)
               -> (f a)
               -> Bool
functorCompose f g x =
  (fmap g $ fmap f x) == fmap (g . f) x

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pure Pair <*> arbitrary <*> arbitrary

data Two a b
  = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = pure Two <*> arbitrary <*> arbitrary

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = pure Three <*> arbitrary <*> arbitrary <*> arbitrary

data Three' a b
  = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = pure Three' <*> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d
  = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         ) => Arbitrary (Four a b d c) where
  arbitrary = pure Four <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Four' a b
  = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = pure Four' <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = (First x)
  fmap f (Second x) = Second (f x)

newtype Mu f = Inf { outF :: f (Mu f) }

-- instance Functor Mu where
--  fmap = undefined

data D = D (Array Word Word) Int Int

data Sum' a b
  = First' b
  | Second' a

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

data Company a b c
  = DeepBlue a b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More a b
  = L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary =
    oneof [ pure Finance
          , pure Desk <*> arbitrary
          , pure Bloor <*> arbitrary
          ]

data K a b
  = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K x) = K x

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = pure K <*> arbitrary

newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = pure Flip <*> arbitrary

data EvilGoateeConst a b
  = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = pure GoatyConst <*> arbitrary

data LiftItOut f a
  = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = pure LiftItOut <*> arbitrary

data Parappa f g a
  = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa (fa) (ga)) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = pure DaWrappa <*> arbitrary <*> arbitrary

data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething (fa) (gb)) = IgnoringSomething (fa) (fmap f gb)

-- Woah. Now I'm even more weirded out.
-- I guess it knows about the combos f a, g b from the type constructor?
instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = pure IgnoringSomething <*> arbitrary <*> arbitrary

data Notorious g o a t
  = Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious (go) (ga) (gt)) =
    Notorious go ga (fmap f gt)

instance ( Arbitrary (g o)
         , Arbitrary (g a)
         , Arbitrary (g t)
         ) => Arbitrary (Notorious g o a t) where
  arbitrary = pure Notorious <*> arbitrary <*> arbitrary <*> arbitrary

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    let
      arbitraryList :: (Arbitrary a) => Int -> Gen (List a)
      arbitraryList s
        | s == 0 = pure Nil
        | s > 10 = arbitraryList 10
        | otherwise = pure Cons <*> arbitrary <*> arbitraryList (s - 1)
    in
      sized arbitraryList

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = oneof [ pure NoGoat
                    , pure OneGoat <*> arbitrary
                    , pure MoreGoats <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read g) = Read (f . g)

-- I still haven't figured out how to test when type can't derive Eq
