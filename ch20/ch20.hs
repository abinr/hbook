import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (x ==))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y) = Just (min x y)


maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y) = Just (max x y)

null :: (Foldable t) => t a -> Bool
null = foldr (\x _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ xs -> xs + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

data Constant a b
  = Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

data Two a b
  = Two a b
  deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x

data Three a b c
  = Three a b c
  deriving (Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

data Three' a b
  = Three' a b b
  deriving (Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y

data Four' a b
  = Four'  a b b b
  deriving (Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f =
  foldMap (\x -> if f x then pure x else mempty)
