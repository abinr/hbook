module PoemLines where

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c xs
  | (head xs == c) = splitOn c (drop 1 xs)
  | otherwise =
      takeWhile (/= c) xs : splitOn c (dropWhile (/= c) xs)

myLines :: String -> [String]
myLines xs = splitOn '\n' xs

myWords :: [Char] -> [[Char]]
myWords xs = splitOn ' ' xs

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In ther forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  ["Tyger Tyger, burning bright"
  ,"In ther forests of the night"
  ,"What immortal hand or eye"
  ,"Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
