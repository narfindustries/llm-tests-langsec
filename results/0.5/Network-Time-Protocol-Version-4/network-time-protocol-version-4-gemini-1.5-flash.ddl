module NetworkTimeProtocolVersion4 where

import Daedalus.Panic

data NTPVersion4 = NTPVersion4 {
  leapIndicator :: {-# UNPACK #-} Word8,
  versionNumber :: {-# UNPACK #-} Word8,
  mode :: {-# UNPACK #-} Word8,
  stratum :: {-# UNPACK #-} Word8,
  pollInterval :: {-# UNPACK #-} Word8,
  precision :: {-# UNPACK #-} Word8,
  rootDelay :: {-# UNPACK #-} Word32,
  rootDispersion :: {-# UNPACK #-} Word32,
  referenceID :: {-# UNPACK #-} Word32,
  referenceTimestamp :: {-# UNPACK #-} Word64,
  originateTimestamp :: {-# UNPACK #-} Word64,
  receiveTimestamp :: {-# UNPACK #-} Word64,
  transmitTimestamp :: {-# UNPACK #-} Word64
}

instance Show NTPVersion4 where
  show (NTPVersion4 li vn mo st pi pr rd rdsp rid rt ot rc tt) =
    "NTPVersion4 {\n" ++
    "  leapIndicator: " ++ show li ++ "\n" ++
    "  versionNumber: " ++ show vn ++ "\n" ++
    "  mode: " ++ show mo ++ "\n" ++
    "  stratum: " ++ show st ++ "\n" ++
    "  pollInterval: " ++ show pi ++ "\n" ++
    "  precision: " ++ show pr ++ "\n" ++
    "  rootDelay: " ++ show rd ++ "\n" ++
    "  rootDispersion: " ++ show rdsp ++ "\n" ++
    "  referenceID: " ++ show rid ++ "\n" ++
    "  referenceTimestamp: " ++ show rt ++ "\n" ++
    "  originateTimestamp: " ++ show ot ++ "\n" ++
    "  receiveTimestamp: " ++ show rc ++ "\n" ++
    "  transmitTimestamp: " ++ show tt ++ "\n" ++
    "}"

parseNTPVersion4 :: Parser NTPVersion4
parseNTPVersion4 = do
  li <- parseWord8
  vn <- parseWord8
  mo <- parseWord8
  st <- parseWord8
  pi <- parseWord8
  pr <- parseWord8
  rd <- parseWord32
  rdsp <- parseWord32
  rid <- parseWord32
  rt <- parseWord64
  ot <- parseWord64
  rc <- parseWord64
  tt <- parseWord64
  return $ NTPVersion4 li vn mo st pi pr rd rdsp rid rt ot rc tt

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

main :: IO ()
main = do
  return ()
