import Data.Monoid
import qualified Data.Semigroup as S
import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = mempty
  mappend Nada Nada = Nada
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only (x `mappend` y)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a
  = First' {getFirst :: Optional a}
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = fmap First' (frequency [(1, pure Nada), (3, fmap Only arbitrary)])

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' (Only x)) (First' Nada) = First' (Only x)
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend (First' (Only x)) (First' (Only _)) = First' (Only x)

main :: IO ()
main = do
  quickCheck
    (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = pure Trivial

semigroupAssoc :: (Eq s, S.Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance S.Semigroup Trivial where
  _ <> _ = Trivial


