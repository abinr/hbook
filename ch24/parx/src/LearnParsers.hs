module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

choose123 :: Parser String
choose123 =
  string "123" <|> string "12" <|> string "1"

str :: CharParsing m => String -> m String
str s = traverse char s

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString (p) mempty "123"

pNL s =
  putStrLn ('\n' : s)

u :: Parser Integer
u =
  pure const <*> integer <*> eof

main = do
  pNL "stop"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- notes from SemVer parsing
-- parseLeadingZerosAsString :: Parser String
-- parseLeadingZerosAsString =
  -- try $ string "0" <> some (alphaNum <|> char '-') <|> some (letter <|> char '-')
