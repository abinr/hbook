module SemVerSpec where

import Test.Hspec
import qualified Text.Trifecta as P
import Data.List (sort)
import SemVer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SemVer Parser" $ do
    it "should parse 2.1.1" $ do
      let m = parx "2.1.1"
      m `shouldBe` Just (SemVer 2 1 1 [] [])
    it "should parse 1.0.0-x.7.z.92" $ do
      let m = parx "1.0.0-x.7.z.92"
          e = SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      m `shouldBe` Just e
    it "should parse 1.0.0-alpha" $ do
      let m = parx "1.0.0-alpha"
      m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] [])
    it "should parse 1.0.0-alpha.1" $ do
      let m = parx "1.0.0-alpha.1"
      m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
    it "should parse 1.0.0-0.3.7" $ do
      let m = parx "1.0.0-0.3.7"
      m `shouldBe` Just (SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] [])
    it "should parse 1.0.0-alpha+001" $ do
      let m = parx "1.0.0-alpha+001"
      m `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] ["001"])
    it "should parse 0.0.1-prealpha.0.31.2+001.wvs" $ do
      let m = parx "0.0.1-pre-alpha.0.31.2+001.w--5vs"
          e = SemVer 0 0 1
                [NOSS "pre-alpha", NOSI 0, NOSI 31, NOSI 2]
                ["001", "w--5vs"]
      m `shouldBe` Just e
    it "should fail leading zeros, 001.0.0" $ do
      let m = parx "001.0.0"
      m `shouldBe` Nothing
    it "should respect major version precedence" $ do
      let lt = parx "1.0.0"
          gt = parx "2.0.0"
      lt `compare` gt `shouldBe` LT
    it "should respect minor version precedence" $ do
      parx "2.0.0" `compare` parx "2.1.0" `shouldBe` LT
    it "should respect patch version precedence" $ do
      parx "2.1.0" `compare` parx "2.1.1" `shouldBe` LT
    it "should assign precedence to a pre-release " $ do
      parx "1.0.0-alpha" `compare` parx "1.0.0" `shouldBe` LT
    it "should assign precedence to pre-release with smaller set" $ do
      parx "1.0.0-alpha" `compare` parx "1.0.0.-alpha.1" `shouldBe` LT
      sort expected `shouldBe` expected

expected = fmap parx
  ["1.0.0-alpha"
  ,"1.0.0-alpha.1"
  ,"1.0.0-alpha.beta"
  ,"1.0.0-beta"
  ,"1.0.0-beta.2"
  ,"1.0.0-beta.11"
  ,"1.0.0-rc.1"
  ,"1.0.0"
  ]


parx :: String -> Maybe SemVer
parx = maybeResult . P.parseString parseSemVer mempty


maybeResult :: P.Result a -> Maybe a
maybeResult (P.Success a) = Just a
maybeResult _ = Nothing
