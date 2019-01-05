module LogParser where

import Text.Trifecta
import Data.Time
import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Control.Monad (void)
import Data.List (groupBy)

main :: IO ()
main = do
  file <- readFile "./src/log.txt"
  case parseString parseFile mempty file of
    Failure err -> print err
    Success xs -> do
     let ds = tagEntriesWithDay xs
     putStr "Total Time Spent: "
     let perDay = (fmap . fmap) sumTimeSpent ds
     putStrLn $ secsToLabel . ceiling $ foldr ((+) . snd) 0 perDay
     putStr "Average Time Per Activity Per Day: "
     putStrLn $ show $ (fmap . fmap) (secsToLabel . avgTimeSpent) ds
     putStrLn $ show (DayLog ds)

sumTimeSpent :: [Entry] -> NominalDiffTime
sumTimeSpent =
  sum . fmap timeSpent . insertEntryEnd

avgTimeSpent :: [Entry] -> Integer
avgTimeSpent es =
  (ceiling . sumTimeSpent $ es) `div` (toInteger . length $ es)

insertEntryEnd :: [Entry] -> [(Entry, Entry)]
insertEntryEnd es =
  zip es (drop 1 es)

timeSpent :: (Entry, Entry) -> NominalDiffTime
timeSpent ((Entry t1 _),(Entry t2 _)) =
  diffUTCTime t2 t1

secsToLabel :: Integer -> String
secsToLabel n =
  let
    (h, h') = n `quotRem` 3600
    (m, s) = h' `quotRem` 60
  in
    show h <> "h " <> show m <> "m " <> show s <> "s"

newtype DayLog =
  DayLog [(Day, [Entry])]

instance Show DayLog where
  show (DayLog []) = []
  show (DayLog (x:xs)) =
    "\n# " <> show (fst x) <> "\n"
    <> unlines (fmap show $ snd x)
    <> show (DayLog xs)

data Entry =
  Entry UTCTime String
  deriving (Eq)

entryTime :: Entry -> UTCTime
entryTime (Entry t _) = t

entryDay :: Entry -> Day
entryDay (Entry t _ ) =
  utctDay t

entryTask :: Entry -> String
entryTask (Entry _ a) = a

instance Show Entry where
  show x =
    (formatTime defaultTimeLocale "%R" $ entryTime x) <> " " <> entryTask x

tagEntriesWithDay :: [Entry] -> [(Day, [Entry])]
tagEntriesWithDay =
  fmap ((,) =<< entryDay . head) . groupBy sameDay

sameDay :: Entry -> Entry -> Bool
sameDay a b =
  entryDay a == entryDay b

parseFile :: Parser [Entry]
parseFile = do
  whiteSpace
  skipMany comment
  fmap concat $ some parseDay

parseDay :: Parser [Entry]
parseDay = do
  symbol "#"
  day <- manyTill anyChar (some newline <|> comment)
  es <- some (parseEntry day)
  return es

parseEntry :: String -> Parser Entry
parseEntry day = do
  time <- count 5 (digit <|>      char ':')
  space
  a <- manyTill anyChar (some newline <|> comment)
  utc <- parseTime' (day <> time)
  return $ Entry (utc) a

parseTime' :: String -> Parser UTCTime
parseTime' time =
  case parseTimeM True defaultTimeLocale "%0F%0R" time of
    Nothing -> unexpected $ "Unable to parse " <> time <> " into UTCTime"
    Just x -> return x

comment :: Parser String
comment = do
  symbol "--"
  c <- manyTill anyChar newline
  whiteSpace
  return c
