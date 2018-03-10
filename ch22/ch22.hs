{-# LANGUAGE InstanceSigs #-}

newtype Reader r a
  = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = pure f <*> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (\r -> x)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (<*>) (Reader f) (Reader g) =
    Reader $ \r -> f r (g r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (>>=) (Reader g) f = Reader $ \r -> (runReader. f) (g r) r

data Person =
  Person {
    humanName :: String
  , dogName :: String
  , address :: String
  } deriving (Show)

data Dog =
  Dog {
    dogsName :: String
  , dogsAddress :: String
  } deriving (Show)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- I have no idea what this exercise wants me to do.
-- I guess maybe  pure Dog <*> dogName <*> address but that wouldn't use the Reader
getDogRM' :: Person -> Dog
getDogRM' p =
  let
    r = Reader Dog
  in
    (runReader r) (dogName p) (address p)
