applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b =
  b
applyTimes n f b =
  f $ applyTimes (n - 1) f b

{--
applyTimes 5 (+1) 5 =
  (+1) $ applyTimes (5 - 1) (+1) 5

(+1) $ (+1) $ applyTimes (4 - 1) (+1) 5

(+1) $ (+1) $ (+1) $ applyTimes (3 - 1) (+1) 5

(+1) $ (+1) $ (+1) $ (+1) $ applyTimes (2 - 1) (+1) 5

(+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes (1 - 1) (+1) 5

(+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5

(+1) $ (+1) $ (+1) $ (+1) $ 6

(+1) $ (+1) $ (+1) $ 7

(+1) $ (+1) $ 8

(+1) $ 9
--}

data DividedResult a
  = Result (a, a)
  | DividedByZero
  deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy num denom = Result $ go num denom 0
  where go n d count
          | n < 0 && d < 0 = go (abs n) (abs d) count
          | n < 0 || d < 0 =
            (\(x, y) -> (negate x, negate y)) $ go (abs n) (abs d) count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)
{--
go 15 2 0
go 13 2 1
go 11 2 2
go 9  2 3
go 7  2 4
go 5  2 5
go 3  2 6
go 1  2 7
(7, 1)
--}

sumNats :: (Eq a, Num a) => a -> a
sumNats 0 = 0
sumNats x =
  x + sumNats (x - 1)

multx :: (Integral a) => a -> a -> a
multx 0 _ = 0
multx _ 0 = 0
multx 1 y = y
multx x 1 = x
multx x y
  | x < 0 && y < 0 = multx (abs x) (abs y)
  | x < 0 || y < 0 = negate $ multx (abs x) (abs y)
  | otherwise =
      y + multx (x - 1) y

mc91 :: (Integral a) => a -> a
mc91 x
     | x > 100 = x - 10
     | x <= 100 =
                 mc91 $ mc91 $ x + 11
