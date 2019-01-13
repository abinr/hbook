module LogParserSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Text.Trifecta as T
import Data.Time
import LogParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogParser Test" $ do
    it "should parse a Diary (DayLog) equal to itself" $
      property propInverse

propInverse :: DayLog -> Bool
propInverse log =
  log == (parseDayLog $ showDayLog log)

f = do
  v <- sample' $ resize 50 (arbitrary :: Gen DayLog)
  let xs = filter (\x -> x /= (parseDayLog $ show x)) v
  traverse (appendFile "fail.txt" . show) xs


parseDayLog :: String -> DayLog
parseDayLog x =
  case T.parseString parseLogFile mempty x of
    T.Failure err -> error $ show err
    T.Success xs ->
     DayLog $ tagEntriesWithDay xs

verboseProperty :: Testable prop => prop -> Expectation
verboseProperty p = verboseCheckResult p >>= (`shouldBe` True) . isSuccess

instance Arbitrary DayLog where
  arbitrary = pure DayLog <*> listOf1 singleDayR

singleDayR :: Gen (Day, [Entry])
singleDayR = do
    d <- dayR
    es <- listOf1 (entryR d) -- why can't I figure out how to do with Applicative?
    return (d, es)

entryR :: Day -> Gen Entry
entryR day =
  pure Entry <*> (timeR day) <*> (listOf1 $ choose ('a', 'z'))


dayR :: Gen Day
dayR =
  let
    year = choose (1998, 2055)
    month = choose (1, 12)
    day = choose (1, 28) -- cheating by limiting range to always valid combinations
  in
    pure fromGregorian <*> year <*> month <*> day

timeR :: Day -> Gen UTCTime
timeR day =
  let
    minute = pure secondsToDiffTime <*> genMinuteSecs
  in
    pure (UTCTime day) <*> minute

genMinuteSecs :: Gen Integer
genMinuteSecs =
  let
    g = (*60) . (flip div) 60
  in
    fmap g $ choose (0, 86400)
