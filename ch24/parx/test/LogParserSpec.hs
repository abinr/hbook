module LogParserSpec where

import Test.QuickCheck
import Data.Time
import LogParser

instance Arbitrary DayLog where
  arbitrary =
    let
      ds = fmap tagEntriesWithDay (arbitrary)
    in
      pure DayLog <*> ds

instance Arbitrary Entry where
  arbitrary = pure Entry <*> timeR <*> arbitrary


timeR :: Gen UTCTime
timeR =
  let
    year = choose (1066, 3250)
    month = choose (1, 12)
    day = choose (1, 28) -- cheating by limiting range to always valid combinations
    date = pure fromGregorian <*> year <*> month <*> day
    hour = pure secondsToDiffTime <*> choose (0, 86400)
  in
    pure UTCTime <*> date <*> hour
