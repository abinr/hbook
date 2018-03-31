module SemVerSpec where

import Test.Hspec
import qualified Text.Trifecta as P
import SemVer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SemVer Parser" $ do
    it "should parse 2.1.1" $ do
      let m = P.parseString parseSemVer mempty "2.1.1"
      maybeResult m `shouldBe` Just (SemVer 2 1 1 [] [])
    it "should parse 1.0.0-x.7.z.92" $ do
      let m = P.parseString parseSemVer mempty "1.0.0-x.7.z.92"
          e = SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      maybeResult m `shouldBe` Just e
    it "should parse 1.0.0-alpha" $ do
      let m = P.parseString parseSemVer mempty "1.0.0-alpha"
      maybeResult m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] [])
    it "should parse 1.0.0-alpha.1" $ do
      let m = P.parseString parseSemVer mempty "1.0.0-alpha.1"
      maybeResult m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
    it "should parse 1.0.0-0.3.7" $ do
      let m = P.parseString parseSemVer mempty "1.0.0-0.3.7"
      maybeResult m `shouldBe` Just (SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] [])
    it "should parse 1.0.0-alpha+001" $ do
      let m = P.parseString parseSemVer mempty "1.0.0-alpha+001"
      maybeResult m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] ["001"])
    it "should parse 0.0.1-prealpha.0.31.2+001.wvs" $ do
      let m = P.parseString parseSemVer mempty "0.0.1-pre-alpha.0.31.2+001.w--5vs"
          e = SemVer 0 0 1
                [NOSS "pre-alpha", NOSI 0, NOSI 31, NOSI 2]
                ["001", "w--5vs"]
      maybeResult m `shouldBe` Just e
    it "should fail leading zeros, 001.0.0" $ do
      let m = P.parseString parseSemVer mempty "001.0.0"
      maybeResult m `shouldBe` Nothing


maybeResult :: P.Result a -> Maybe a
maybeResult (P.Success a) = Just a
maybeResult _ = Nothing
