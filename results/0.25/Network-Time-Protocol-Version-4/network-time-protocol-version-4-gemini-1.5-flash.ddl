module NetworkTimeProtocolVersion4 where

import Daedalus.TH

{-@ type NTPVersion4 = { version :: { major :: Word8, minor :: Word8 }
                       , stratum :: Word8
                       , poll :: Word8
                       , precision :: Word8
                       , rootDelay :: Word32
                       , rootDispersion :: Word32
                       , referenceID :: Word32
                       , referenceTimestamp :: Word64
                       , originTimestamp :: Word64
                       , receiveTimestamp :: Word64
                       , transmitTimestamp :: Word64
                       } @-}

data NTPVersion4 = NTPVersion4
  { version :: { major :: Word8, minor :: Word8 }
  , stratum :: Word8
  , poll :: Word8
  , precision :: Word8
  , rootDelay :: Word32
  , rootDispersion :: Word32
  , referenceID :: Word32
  , referenceTimestamp :: Word64
  , originTimestamp :: Word64
  , receiveTimestamp :: Word64
  , transmitTimestamp :: Word64
  }

instance Show NTPVersion4 where
  show (NTPVersion4 v s p prec rd rdsp rid rt ot rt t) =
    unlines
      [ "Version: " ++ show v
      , "Stratum: " ++ show s
      , "Poll: " ++ show p
      , "Precision: " ++ show prec
      , "Root Delay: " ++ show rd
      , "Root Dispersion: " ++ show rdsp
      , "Reference ID: " ++ show rid
      , "Reference Timestamp: " ++ show rt
      , "Origin Timestamp: " ++ show ot
      , "Receive Timestamp: " ++ show rt
      , "Transmit Timestamp: " ++ show t
      ]


parseNTPVersion4 :: Parser NTPVersion4
parseNTPVersion4 = do
  v <- parseVersion
  s <- getWord8
  p <- getWord8
  prec <- getWord8
  rd <- getWord32
  rdsp <- getWord32
  rid <- getWord32
  rt <- getWord64
  ot <- getWord64
  rcv <- getWord64
  t <- getWord64
  return $ NTPVersion4 v s p prec rd rdsp rid rt ot rcv t


parseVersion :: Parser { major :: Word8, minor :: Word8 }
parseVersion = do
  versionBytes <- getBytes 2
  let major = fromIntegral (fromEnum (versionBytes !! 0))
  let minor = fromIntegral (fromEnum (versionBytes !! 1))
  return { major = major, minor = minor }


getWord8 :: Parser Word8
getWord8 = fromIntegral <$> getByte

getWord32 :: Parser Word32
getWord32 = fromIntegral <$> getBytes4

getWord64 :: Parser Word64
getWord64 = fromIntegral <$> getBytes8

getBytes :: Int -> Parser [Word8]
getBytes n = replicateM n getByte

getBytes4 :: Parser Word32
getBytes4 = do
  a <- getByte
  b <- getByte
  c <- getByte
  d <- getByte
  return $ (fromIntegral a) .|. (fromIntegral b `shiftL` 8) .|. (fromIntegral c `shiftL` 16) .|. (fromIntegral d `shiftL` 24)

getBytes8 :: Parser Word64
getBytes8 = do
  a <- getByte
  b <- getByte
  c <- getByte
  d <- getByte
  e <- getByte
  f <- getByte
  g <- getByte
  h <- getByte
  return $ (fromIntegral a) .|. (fromIntegral b `shiftL` 8) .|. (fromIntegral c `shiftL` 16) .|. (fromIntegral d `shiftL` 24) .|. (fromIntegral e `shiftL` 32) .|. (fromIntegral f `shiftL` 40) .|. (fromIntegral g `shiftL` 48) .|. (fromIntegral h `shiftL` 56)


main :: IO ()
main = do
  return ()
