import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1923 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  let
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f x xs =
      case x of
        DbDate d -> [d] ++ xs
        _ -> xs
  in
    foldr f []

{-- probably very naive --}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (\(DbDate x) -> x) . maximum

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = maximum . filterDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  let
    f :: DatabaseItem -> [Integer] -> [Integer]
    f x xs =
      case x of
        DbNumber d -> [d] ++ xs
        _ -> xs
  in
    foldr f []

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs =
  let
    ds = filterDbNumber xs
  in
    fromIntegral (sum ds) / fromIntegral (length ds)
