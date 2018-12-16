module PhoneParserSpec where

import Test.Hspec
import qualified Text.Trifecta as P
import PhoneParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PhoneParserSpec" $ do
    it "should parse area-exchange-line" $ do
      let m = parx "123-456-7890"
      m `shouldBe` Just (PhoneNumber "123" "456" "7890")
    it "should parse areaexchangeline" $ do
      let m = parx "1234567890"
      m `shouldBe` Just (PhoneNumber "123" "456" "7890")
    it "should parse (area) exchange-line" $ do
      let m = parx "(123) 456-7890"
      m `shouldBe` Just (PhoneNumber "123" "456" "7890")
    it "should parse country-area-exchange-line" $ do
      let m = parx "1-123-456-7890"
      m `shouldBe` Just (PhoneNumber "123" "456" "7890")

parx :: String -> Maybe PhoneNumber
parx = maybeResult . P.parseString parsePhone mempty


maybeResult :: P.Result a -> Maybe a
maybeResult (P.Success a) = Just a
maybeResult _ = Nothing
