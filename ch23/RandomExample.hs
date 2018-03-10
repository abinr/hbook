module RandomExample where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    _ -> error $ "wat? " ++ show n

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes =
  pure (,,) <*> rollDie <*> rollDie <*> rollDie

rollDie :: State StdGen Die
rollDie =
  fmap intToDie . state $ randomR (1, 6)

nDie :: Int -> State StdGen [Die]
nDie n =
  replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let
          (die, nextGen) =
            randomR (1, 6) gen
        in
          go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, ds) gen
      | sum >= n = (count, ds)
      | otherwise =
        let
          (die, nextGen) =
            randomR (1, 6) gen
        in
          go (sum + die) (count + 1, (intToDie die) : ds) nextGen
