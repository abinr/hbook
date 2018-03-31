{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Text.RawString.QQ
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative
import Data.Scientific (floatingOrInteger)

main :: IO ()
main = do
  print $ (decode sectionJson :: Maybe TestData)

data TestData
  = TestData {
      section :: Host
    , what :: Color
    } deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = pure TestData <*> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

newtype Host
  = Host String
  deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = pure Host <*> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

type Annotation = String

data Color
  = Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON Color where
  parseJSON (Object v) =
        pure Red <*> v .: "red"
    <|> pure Blue <*> v.: "blue"
    <|> pure Yellow <*> v.: "yellow"

data NumberOrString
  = Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Left _  -> fail "Must be integral number"
      Right x -> pure $ Numba x
  parseJSON (String s) = pure $ Stringy s
  parseJSON _ = fail "NumberOrString must be number or string"

sectionJson :: LBS.ByteString
sectionJson = [r|
               { "section": {"host": "wikipedia.ord"},
                 "whatisit": {"red": "intoothandclaw"}
               }
              |]
