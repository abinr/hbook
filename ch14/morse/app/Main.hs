module Main where

import Morse (stringToMorse, morseToChar)
import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  done <- hIsEOF stdin
  when done exitSuccess

  line <- hGetLine stdin
  case traverse morseToChar (words line) of
    (Just s) ->
      putStrLn s
    Nothing -> do
      putStrLn $ "ERROR: " ++ line
      exitFailure

convertToMorse :: IO ()
convertToMorse = forever $ do
  done <- hIsEOF stdin
  when done exitSuccess

  line <- hGetLine stdin
  case stringToMorse line of
    (Just s) ->
      putStrLn (intercalate " " s)
    Nothing -> do
      putStrLn $ "ERROR: " ++ line
      exitFailure


main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError


argError :: IO ()
argError = do
  putStrLn $ "Please specify the "
           ++ "first argument as being 'from' or 'to' morse, "
           ++ "such as: morse to"
  exitFailure

