module Test where

import Test.Hspec
import Main

test :: IO ()
test = hspec $ do
  describe "Word Guess" $ do
    it "should handle a guess already guessed" $ do
      let p = (Puzzle "" "h")
      newP <- handleGuess p 'h'
      newP `shouldBe` p
    it "should handle a correct guess" $ do
      let p = (Puzzle "secret" "")
      newP <- handleGuess p 'e'
      newP `shouldBe` (Puzzle "secret" "e")
    it "should handle an incorrect guess" $ do
      let p = (Puzzle "secret" "")
      newP <- handleGuess p 'z'
      newP `shouldBe` (Puzzle "secret" "z")

