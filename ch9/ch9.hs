import Data.Char (isUpper, toUpper)

myFilter :: String -> [String]
myFilter =
  let
    as = ["the", "a", "an"]
  in
    filter (not . flip elem as) . words

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) =
  (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) =
  f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

onlyUpper :: String -> String
onlyUpper =
  filter isUpper

firstUpper :: String -> String
firstUpper [] = []
firstUpper (x:xs) =
  toUpper x : xs

allUpper :: String -> String
allUpper [] = []
allUpper (x:xs) =
  toUpper x : allUpper xs

first :: String -> Maybe Char
first [] = Nothing
first (x:_) =
  Just $ toUpper x

first' :: String -> Char
first' xs =
  toUpper . head $ xs

first'' :: String -> Char
first'' =
  toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
  if f x == True
  then True
  else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) =
  if e == x
  then True
  else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs =
  any (== e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) =
  myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) =
  x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) =
  f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f (x:y:xs) =
  case f x y of
    LT -> myMaximumBy f (y:xs)
    EQ -> myMaximumBy f (y:xs)
    GT -> myMaximumBy f (x:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:y:xs) =
  case f x y of
    LT -> myMinimumBy f (x:xs)
    EQ -> myMinimumBy f (y:xs)
    GT -> myMinimumBy f (y:xs)

myMaximum :: Ord a => [a] -> a
myMaximum =
  myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum =
  myMinimumBy compare
