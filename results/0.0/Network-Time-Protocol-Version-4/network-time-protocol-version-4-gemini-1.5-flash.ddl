module NetworkTimeProtocolVersion4 where

import Daedalus.Panic

data NTPVersion4 = NTPVersion4 {
  leapIndicator :: {-# UNPACK #-} !Word8,
  versionNumber :: {-# UNPACK #-} !Word8,
  mode :: {-# UNPACK #-} !Word8,
  stratum :: {-# UNPACK #-} !Word8,
  poll :: {-# UNPACK #-} !Word8,
  precision :: {-# UNPACK #-} !Word8,
  rootDelay :: {-# UNPACK #-} !Word32,
  rootDispersion :: {-# UNPACK #-} !Word32,
  referenceID :: {-# UNPACK #-} !Word32,
  referenceTimestamp :: {-# UNPACK #-} !Word64,
  originateTimestamp :: {-# UNPACK #-} !Word64,
  receiveTimestamp :: {-# UNPACK #-} !Word64,
  transmitTimestamp :: {-# UNPACK #-} !Word64
}

parseNTPVersion4 :: Parser NTPVersion4
parseNTPVersion4 = do
  leapIndicator <- parseWord8
  versionNumber <- parseWord8
  mode <- parseWord8
  stratum <- parseWord8
  poll <- parseWord8
  precision <- parseWord8
  rootDelay <- parseWord32
  rootDispersion <- parseWord32
  referenceID <- parseWord32
  referenceTimestamp <- parseWord64
  originateTimestamp <- parseWord64
  receiveTimestamp <- parseWord64
  transmitTimestamp <- parseWord64
  return $ NTPVersion4 {
    leapIndicator = leapIndicator,
    versionNumber = versionNumber,
    mode = mode,
    stratum = stratum,
    poll = poll,
    precision = precision,
    rootDelay = rootDelay,
    rootDispersion = rootDispersion,
    referenceID = referenceID,
    referenceTimestamp = referenceTimestamp,
    originateTimestamp = originateTimestamp,
    receiveTimestamp = receiveTimestamp,
    transmitTimestamp = transmitTimestamp
  }

parseWord8 :: Parser Word8
parseWord8 = do
  x <- bytes 1
  return $ fromIntegral (fromList x)

parseWord32 :: Parser Word32
parseWord32 = do
  x <- bytes 4
  return $ fromIntegral (fromList x)

parseWord64 :: Parser Word64
parseWord64 = do
  x <- bytes 8
  return $ fromIntegral (fromList x)

fromList :: [Word8] -> Word64
fromList xs = foldl' (\acc x -> acc * 256 + fromIntegral x) 0 xs

main :: Daedalus.Parser.Parser NTPVersion4
main = parseNTPVersion4
