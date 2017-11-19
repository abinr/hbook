lefts' :: [Either a b] -> [a]
lefts' =
  let
    f (Left x) xs = x : xs
    f (Right x) xs = xs
  in
    foldr f []


rights' :: [Either a b] -> [b]
rights' =
  let
    f (Left x) xs = xs
    f (Right x) xs = x : xs
  in
    foldr f []


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs =
  (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' l _ (Left a) = l a
either' _ r (Right b) = r b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f =
  either' (const Nothing) (Just . f)
