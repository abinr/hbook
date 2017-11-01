exclamate :: String -> String
exclamate =
  flip (++) "!"

fifth :: String -> String
fifth =
  take 1 . drop 4

fromTenth :: String -> String
fromTenth =
  drop 9

thirdLetter :: String -> Char
thirdLetter =
  flip (!!) 2

letterIndex =
  (!!) "Curry is awesome!"
