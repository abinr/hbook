import Data.Bool (bool)

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f acc [] = acc
myfoldl f acc (x:xs) = foldl f (f acc x) xs

{--
myfoldl (flip (*)) 1 [1..3]

myfoldl (flip (*)) ((flip (*)) 1 1) [2..3]

myfoldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]

myfoldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []

6
--}


stops = "pbtdkg"
vowels = "aeiou"


onlyPs =
  filter (\(a, _, _) -> a == 'p') $ combos stops vowels

nouns = ["blanket", "forest", "mouth", "smile", "sweat"]
verbs = ["enjoys", "remembers", "imagines"]

combos :: [a] -> [b] -> [(a, b, a)]
combos xs ys =
  functionMap (functionMap (map (,,) xs) ys) xs

functionMap :: [(a -> b)] -> [a] -> [b]
functionMap _ [] = []
functionMap [] _ = []
functionMap (f:fs) xs =
  map f xs ++ functionMap fs xs

seekrit :: Fractional a => String -> a
seekrit x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr =
  foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny =
  flip foldr False . ((||) . )

myElem :: Eq a => a -> [a] -> Bool
myElem =
  flip foldr False . ((||) .) . (==)

myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse =
  foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap =
  flip foldr [] . ((:) . )

{-- Pass on pointfree. Ouch! --}
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr (\x xs -> if f x then (x:xs) else xs) []

squish :: [[a]] -> [a]
squish =
  foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap =
  flip foldr [] . ((++) . )

squishAgain :: [[a]] -> [a]
squishAgain =
  squishMap id

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' _ [x] = x
myMaximumBy' f (x : y : xs) =
  case f x y of
    LT -> myMaximumBy f (y:xs)
    EQ -> myMaximumBy f (y:xs)
    GT -> myMaximumBy f (x:xs)

{-- Pass on pointfree. Ouch! --}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) =
  foldl (\acc a -> if f acc a == GT then acc else a) x xs

{-- Pass on pointfree. Ouch! --}
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) =
  foldl (\acc a -> if f acc a == LT then acc else a) x xs
