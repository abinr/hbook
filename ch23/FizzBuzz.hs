module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State


main :: IO ()
main = do
  mapM_ putStrLn $ fizzBuzzFromTo 1 100

fb :: (Show a, Integral a) => a -> String
fb n =
  case (n `rem` 3, n `rem` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _ -> show n

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = fizzBuzzList $ enumFromThenTo to (to - 1) from

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fb n
  put (result : xs)
