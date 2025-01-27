module GZIP.Gemini (..) where

import Daedalus.Type.AST
import Daedalus.Panic

data GzipHeader = GzipHeader {
  id :: {member :: ByteString 10}
}

data GzipBlock = GzipBlock {
  header :: GzipHeader,
  data :: ByteString
}

gzipBlockParser :: Parser GzipBlock
gzipBlockParser = do
  header <- parse GzipHeader
  data <- parseByteString
  return GzipBlock{header, data}

parse GzipHeader = do
  id <- bytes 10
  return GzipHeader{id}


parseByteString :: Parser ByteString
parseByteString = do
  len <- getUInt32be
  bytes (fromIntegral len)


getUInt32be :: Parser UInt32
getUInt32be = do
  bs <- bytes 4
  let val = fromIntegral (fromByte bs !! 0) * 2^24 +
            fromIntegral (fromByte bs !! 1) * 2^16 +
            fromIntegral (fromByte bs !! 2) * 2^8 +
            fromIntegral (fromByte bs !! 3)
  return (fromIntegral val)

fromByte :: Byte -> UInt32
fromByte b = fromIntegral (ord b)

main :: Parser ()
main = do
  block <- gzipBlockParser
  return ()


