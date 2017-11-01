squareThenPi :: Floating a => a -> a
squareThenPi =
  (* pi) . (^ 2)
  
times3ThenAdd = x * 3 + y
      where x = 3
            y = 1000

mixItUpALot = x * 5
    where x = 10 * 5 + y
          y = 10

divideMeWatevs = z / x + y
       where x = 7
             y = negate x
             z = y * 10

waxOn     = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff = triple . (+10) . (subtract 50)
