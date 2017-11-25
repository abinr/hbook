module Main where

import Control.Monad (forever)
import Data.Char (toLower, isLetter)
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Control.Applicative (liftA2)
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter onlyLetters $ filter between aw
  where
    between x = (length x) `elem` (enumFromTo 5 8)
    onlyLetters x = all isLetter x

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' =
  gameWords >>= randomWord

type Solution = String
type Guesses = [Char]

data Puzzle = Puzzle Solution Guesses

instance Show Puzzle where
  show puzzle =
    "================================"
    ++ "\nCurrent puzzle is: "
    ++ intersperse ' ' (fmap renderPuzzleChar $ guessed puzzle)
    ++ "\nMistakes: " ++ (mistakes puzzle)
    ++ "\nGuesses Left: " ++ (show $ remainingGuesses puzzle)
    ++ "\n================================"

remainingGuesses :: Puzzle -> Int
remainingGuesses puzzle@(Puzzle w _) =
  length w - (length $ mistakes puzzle)

guessed :: Puzzle -> [Maybe Char]
guessed (Puzzle s g) =
  let
    f x =
      if x `elem` g
      then (Just x)
      else Nothing
  in
    fmap f s

mistakes :: Puzzle -> [Char]
mistakes (Puzzle s g) =
  filter (\x -> not $ x `elem` s) g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

isSolved :: Puzzle -> Bool
isSolved puzzle@(Puzzle word _) =
  word == (catMaybes $ guessed puzzle)

freshPuzzle :: String -> Puzzle
freshPuzzle x =
  Puzzle x []

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle word guesses) guess = do
  case (guess `elem` guesses, guess `elem` word) of
    (True, _) -> do
      putStrLn $ "You already guessed " ++ [guess] ++ ", pick something else"
      return puzzle
    (_, True) -> do
      putStrLn $ [guess] ++ " is a great guess!"
      return $ Puzzle word (guess : guesses)
    (_, False) -> do
      putStrLn $ "Sorry! " ++ [guess] ++ " is not in word."
      return $ Puzzle word (guess : guesses)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _) =
  if remainingGuesses puzzle < 1
  then do putStrLn "You Lose"
          putStrLn $ "The word was " ++ wordToGuess
          exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin puzzle@(Puzzle wordToGuess _) =
  if isSolved puzzle then
    do putStrLn "You win!"
       putStrLn $ "The word was " ++ wordToGuess
       exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn "Your guess must be a single char."


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
