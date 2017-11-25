import Control.Monad
import System.Exit
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let t = map toLower . filter isLetter $ line1
  case (t == reverse t) of
    True -> do
      putStrLn "It's a palindrome."
    False -> do
      putStrLn "Nope!"
      exitSuccess
