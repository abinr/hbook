
module Data.Ini where

import Control.Applicative
import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Text.Trifecta

data Section
  = Section Header Assignments
  deriving (Eq, Show)

newtype Config
  = Config (M.Map Header Assignments)
  deriving (Eq, Show)

newtype Header
  = Header String
  deriving (Eq, Ord, Show)

type Assignments = M.Map Name Value
type Name = String
type Value = String

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

rollup :: Section -> M.Map Header Assignments -> M.Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

parseHeader :: Parser Header
parseHeader =
  char '[' *> (Header <$> some letter) <* char ']'

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany $ do
  char ';' <|> char '#'
  skipMany (noneOf "\n")
  skipEOL


