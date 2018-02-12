module BadMonoid where

import Data.Monoid

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools
