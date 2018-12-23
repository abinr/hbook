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
      putStrLn "Total Time Spent per Day"
      putStrLn $ unlines $ fmap show $ fmap sumTimeSpent $ tagEntriesWithDay xs

sumTimeSpent :: DayLog -> (Day, NominalDiffTime)
sumTimeSpent (DayLog d es) =
  zip es (drop 1 es)
  |> fmap timeSpent
  |> sum
  |> (,) d

data Entry =
  Entry UTCTime String
  deriving (Eq, Show)

data DayLog =
  DayLog Day [Entry]
  deriving (Eq, Show)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

tagEntriesWithDay :: [Entry] -> [DayLog]
tagEntriesWithDay entries =
  entries
  |> groupBy sameDay
  |> fmap (DayLog =<< entryDay . head) --too glyphic?

entryDay (Entry t _ ) =
  utctDay t

sameDay :: Entry -> Entry -> Bool
sameDay a b =
  entryDay a == entryDay b

timeSpent :: (Entry, Entry) -> NominalDiffTime
timeSpent ((Entry t1 _),(Entry t2 _)) =
  diffUTCTime t2 t1

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
  time <- count 5 (digit <|> char ':')
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
