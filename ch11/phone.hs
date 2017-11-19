module Phone where

import Data.Char (isUpper, toLower, isLetter)
import Data.List (elemIndex, maximumBy, group, sort)

type DaPhone = [(Char, String)]
type Digit = Char
type Presses = Int

coolestWord :: [String] -> String
coolestWord =
  head
  . maximumBy (\x y -> compare (length x) (length y))
  . group . sort . concat . map words

coolestLtr :: [String] -> Char
coolestLtr =
  mostPopularLetter . filter isLetter . concat

-- doesn't account for ties
mostPopularLetter :: String -> Char
mostPopularLetter =
  head . maximumBy (\x y -> compare (length x) (length y)) . group . sort

letterCost :: Char -> Presses
letterCost =
  fingerTaps . reverseTaps keymap

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps =
  sum . map snd

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone =
  concatMap (reverseTaps phone)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c =
  if isUpper c
  then [('*', 1)] ++ quest (toLower c) phone
  else quest c phone

quest :: Char -> DaPhone -> [(Digit, Presses)]
quest c =
  map (\(d, Just p) -> (d, p + 1))
  . filter ((/= Nothing) . snd)
  . map (\(k, v) -> (k, c `elemIndex` v))

keymap :: [(Char, String)]
keymap =
    [ ('0', " ")
    , ('1', "1")
    , ('2', "abc2")
    , ('3', "def3")
    , ('4', "ghi4")
    , ('5', "jkl5")
    , ('6', "mno6")
    , ('7', "qrs7")
    , ('8', "tuv8")
    , ('9', "wxyz9")
    , ('#', ".,")
    ]

convo :: [String]
convo = 
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool. Ur turn"
  , "Ok. Do u thing I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
