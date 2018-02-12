module Typez where

import Data.Monoid ((<>))

-- []
pureList :: a -> [a]
pureList x = [x]

apList :: [(a -> b)] -> [a] -> [b]
apList [] _ = []
apList _ [] = []
apList (f:fs) xs =
  fmap f xs <> (fs <*> xs)


-- IO
pureIO :: a -> IO a
pureIO = pure -- probably cheating

apIO :: IO (a -> b) -> IO a -> IO b
apIO = (<*>)

-- (,)
-- Interesting. pure (++) <*> ("1", "2") can't work
-- without Applicative implementation of ((->) r). Duh
pureTuple :: Monoid a => a -> (a, a)
pureTuple x = (mempty, x)

apTuple :: Monoid a => (a, a -> b) -> (a, a) -> (a, b)
apTuple (z, f) (y, x) = (z <> y, f x)

-- (->) e
pureFunc :: a -> (e -> a)
pureFunc = const -- mind blown (peeked on CHC.Base)

apFunc :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
apFunc f g x = f x (g x) -- mind doubly blown
-- the x is the e in (e -> b), so clever yet so simple

-- going to leave a test for later :-)
