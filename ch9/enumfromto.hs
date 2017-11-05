ranger :: Enum a => a -> a -> [a]
ranger start stop
  | a < b = start : ranger (toEnum $ 1 + a) stop
  | a == b = [start]
  | a > b = []
  where
    a = fromEnum start
    b = fromEnum stop

eftBool :: Bool -> Bool -> [Bool]
eftBool = ranger

eftOrd :: Ordering ->
          Ordering ->
          [Ordering]
eftOrd = ranger

eftInt :: Int -> Int -> [Int]
eftInt = ranger

eftChar :: Char -> Char -> [Char]
eftChar = ranger
