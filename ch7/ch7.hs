avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

h :: Ord a => a -> a -> Bool
h = (==)

tensDigit :: Integral a => a -> a
tensDigit = flip mod 10 . fst . flip divMod 10

hunsD :: Integral a => a -> a
hunsD = flip mod 100 . flip div 100

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> y
    False -> x

foldBool' x y b
  | b == True = y
  | b == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) =
  (f a, c)
