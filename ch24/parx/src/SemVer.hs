module SemVer where

import Text.Trifecta
import Control.Applicative
import Data.Semigroup
import Numeric.Natural
import Data.Char (digitToInt)

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

type Major = Natural
type Minor = Natural
type Patch = Natural
type Release = [NumberOrString]
type Metadata = [String]

data NumberOrString
  = NOSS String
  | NOSI Natural
  deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- strictNatural
  char '.'
  minor <- strictNatural
  char '.'
  patch <- strictNatural
  optional (char '-')
  releases <- parseNumberOrString `sepEndBy` (symbol ".")
  optional (char '+')
  metadata <- (some $ alphaNum <|> char '-') `sepEndBy` (symbol ".")
  pure $ SemVer
    (major) (minor) (patch) releases metadata

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  NOSI . fromInteger <$> natural <|> NOSS <$> some (alphaNum <|> char '-')


-- matches any natural number, leading zeros not allowed
-- .. but I need a Natural not a String
strictNatural :: Parser Natural
strictNatural =
  loneZero <|> fmap (fromInteger . listToInteger) ints

loneZero :: Parser Natural
loneZero = (char '0' <* notFollowedBy alphaNum) *> pure 0

naturals :: Parser String
naturals = pure (:) <*> (satisfyRange '1' '9') <*> (many digit)

ints :: Parser [Integer]
ints = (fmap . fmap) (toInteger . digitToInt) naturals

listToInteger :: [Integer] -> Integer
listToInteger xs = foldl1 (\acc a -> acc * 10 + a) xs
