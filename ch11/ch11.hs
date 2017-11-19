{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Data.List

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

newtype IntString = IntString (Int, String) deriving Show


instance TooMany IntString where
  tooMany (IntString (i, s)) = i > 10

newtype IntPair = IntPair (Int, Int) deriving Show

instance TooMany IntPair where
  tooMany (IntPair (i, j)) = i + j > 47

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)

{--
instance TooMany Goats where
  tooMany (Goats x) = tooMany x
--}



type AuthorName = String

data Author
  = Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)


type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show


data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b
  = RecordProduct
  { pFirst :: a
  , psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)

newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

newtype NumSheep
  = NumSheep Int
  deriving (Eq, Show)

data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Eq, Show)

data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo
  = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Show)

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , Purescript
  ]

allProgrammers :: [Programmer]
allProgrammers =
  let
    functionMap :: [(a -> b)] -> [a] -> [b]
    functionMap [] _ = []
    functionMap (f:fs) xs =
      map f xs ++ functionMap fs xs
  in
    functionMap (map Programmer allOperatingSystems) allLanguages

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = False
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = True
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = True
convert8 No = True
convert8 Both = False

{-- Why do I need the as-pattern? Tried for 1.5 hrs to figure it out, no avail --}
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf xs ys =
  xs == concat (foldr (\a b -> filter (== a) xs : b) [] ys)

subseqOfTest =
  map (isSubseqOf "blah")
  [ "blahwoot"
  , "wootblah"
  , "wboloath"
  , "wootbla"
  , "halbwoot"
  , "blawhoot"
  ]

capitalizeWords :: String -> [(String, String)]
capitalizeWords s =
  let
    f t@(x:xs) =
      (t, (toUpper x) : xs)
  in
    map f $ words s

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) =
  (toUpper x) : xs

s = "blah. woot ha"

capitalizeParagraph :: String -> String
capitalizeParagraph =
  reverse
  . ((:) '.')
  . reverse
  . intercalate ". "
  . map (capitalizeWord . dropWhile (== ' '))
  . sentences

sentences :: String -> [String]
sentences [] = []
sentences ('.':xs) =
  sentences xs
sentences xs =
  takeWhile (/= '.') xs : sentences (dropWhile (/= '.') xs)
