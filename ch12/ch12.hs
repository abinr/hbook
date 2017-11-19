import Data.List

replaceThe :: String -> String
replaceThe [] = []
replaceThe xs =
  let
    (w:ws) = words xs
    f s =
      case notThe s of
        Just x -> x
        Nothing -> "a"
  in
    f w ++ " " ++ replaceThe (concat . intersperse " " $ ws)


notThe :: String -> Maybe String
notThe x =
  if x /= "the"
  then Just x
  else Nothing

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel ('t':'h':'e':' ':x:xs) =
  if x `elem` "aeiou"
  then 1 + countTheBeforeVowel xs
  else countTheBeforeVowel xs
countTheBeforeVowel (x:xs) =
  countTheBeforeVowel xs


countVowels :: String -> Integer
countVowels =
  let
    f a b =
      if a `elem` "aeiou"
      then 1 + b
      else 0 + b
  in
    foldr f 0

newtype Word'
  = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs =
  let
      v = length $ filter (flip elem vowels) xs
      c = length xs - v
  in
    if v > c
    then Nothing
    else Just (Word' xs)

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) =
  1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i =
  let
    go 0 = Zero
    go n = Succ $ go (n - 1)
  in
    if i < 0
    then Nothing
    else Just (go i)

myIterate :: (a -> a) -> a -> [a]
myIterate f z =
  z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z =
  case f z of
    Just (x, y) ->
      x : myUnfoldr f y
    Nothing ->
      []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z =
  myUnfoldr (\b -> Just (b, f b)) z

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z =
  case f z of
    Just (a, b, c) ->
      Node (unfold f a) b (unfold f c)
    Nothing ->
      Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  let
    f x =
      if x >= n
      then Nothing
      else Just (x + 1, x, x + 1)
  in
    unfold f 0
