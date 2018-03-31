{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.IniSpec where

import Test.Hspec
import Text.Trifecta
import Data.ByteString (ByteString)
import Text.RawString.QQ
import qualified Data.Map as M

import Data.Ini

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString parseAssignment mempty ("woot=1" :: ByteString)
      maybeResult m `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty "[blah]"
      maybeResult m `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
      maybeResult m `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          states = M.fromList [("Chris", "Texas")]
      maybeResult m `shouldBe` Just (Section (Header "states") states)

  describe "INI parsing" $
    it "can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
      maybeResult m `shouldBe` iniExpected

maybeResult :: Result a -> Maybe a
maybeResult (Success a) = Just a
maybeResult _ = Nothing

sectionEx :: ByteString
sectionEx = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

iniExpected :: Maybe Config
iniExpected =
  let
    sectionValues =
      M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
    whatisitValues =
      M.fromList [("red", "intoothandclaw")]
  in
    Just $ Config $
      M.fromList
        [ (Header "section", sectionValues)
        , (Header "whatisit", whatisitValues)
        ]
