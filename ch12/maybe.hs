isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just x) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just x) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f Nothing = z
mayybee z f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes =
  let
    f Nothing xs = xs
    f (Just x) xs = x : xs
  in
    foldr f []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : xs) = Nothing
flipMaybe ((Just x) : xs ) =
  maybeMap (x : ) (flipMaybe xs)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)
