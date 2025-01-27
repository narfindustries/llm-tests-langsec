module GZIP.Gemini (..) where

import Daedalus.Type.AST

data GzipHeader = GzipHeader {
  id :: { id :: Word8 }
}

data GzipBlock = GzipBlock {
  btype :: { btype :: Word8 }
}

data GzipTrailer = GzipTrailer {
  crc :: { crc :: Word32 }
}

gzip_gemini :: forall a . Daedalus a
gzip_gemini = do
  header <- GzipHeader <$> (word8 0x1f) <*> (word8 0x8b)
  block <- GzipBlock <$> word8
  trailer <- GzipTrailer <$> word32be
  return ()


