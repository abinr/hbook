module PhoneParser where

import Text.Trifecta
import Control.Applicative

type Area = String
type Exchange = String
type LineNumber = String

data PhoneNumber =
  PhoneNumber Area Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional $ string "1-"
  optional $ char '('
  area <- count 3 digit
  optional $ char ')'
  optional space
  optional $ char '-'
  exchg <- count 3 digit
  optional $ char '-'
  line <- count 4 digit
  return $ PhoneNumber area exchg line


