module IPv4Parser where

import Text.Trifecta
import Data.Word
import Data.Char
import Control.Applicative

data IPv4Address =
  IPv4Address Word32
  deriving (Eq, Ord, Show)

parseIPv4Address :: Parser IPv4Address
parseIPv4Address =
  fmap (IPv4Address . octetListToInteger) $ some parseOctet

parseOctet :: Parser Word8
parseOctet = do
  n <- natural <* optional (char '.')
  case n <= 255 of
    False -> unexpected $ "Octet cannot be > 255. You have " ++ show n
    True -> return $ fromIntegral n

octetListToInteger :: [Word8] -> Word32
octetListToInteger =
  foldl1 (\acc a -> acc * 256 + a) . fmap fromIntegral

