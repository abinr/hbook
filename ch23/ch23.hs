import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Moi s a
  = Moi { runMoi :: s -> (a, s) }

instance (Show s, Show a) => Show (Moi s a) where
  show (Moi f) = "<unprintable>"

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s ->  let (x, y) = g s in (f x, y)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
            let
              (h, s') = f s
              (a, s'') = g s'
            in
              (h a, s'')

instance Monoid s => Monad (Moi s) where
  (Moi f) >>= g =
    Moi $ \s ->
            let
              (a, s') = f s
              (Moi h) = g a
            in
              h s'

instance ( Arbitrary s
         , CoArbitrary s
         , Arbitrary a)
         => Arbitrary (Moi s a) where
  arbitrary = pure Moi <*> arbitrary

instance (Show s, Arbitrary s, EqProp s, EqProp a) => EqProp (Moi s a) where
  (Moi f) =-= (Moi g) = f =-= g

get :: Moi s s
get =  Moi $ \s -> (s, s)

put :: s -> Moi s ()
put = \s -> Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
