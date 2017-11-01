data TisAnInteger =
  TisAn Integer

instance Eq (TisAnInteger) where
  (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers =
  Two Integer Integer

instance Eq (TwoIntegers) where
  Two a b == Two c d =
    a == c && b == d

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq (StringOrInt) where
  TisAnInt x == TisAnInt y = x == y
  TisAString x == TisAString y = x == y
  TisAnInt _ == TisAString _ = False
  TisAString _ == TisAnInt _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' =
    x == x' && y == y'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' =
    x == x' && y == y'

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y
  ThisOne _ == ThatOne _ = False
  ThatOne _ == ThisOne _ = False

data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  Hello _ == Goodbye _ = False
  Goodbye _ == Hello _ = False

