module GZIP.Gemini (..) where

import Daedalus.Type.AST

data GzipHeader = GzipHeader {
  id :: { id :: Integer }
}

data GzipTrailer = GzipTrailer {
  crc32 :: { crc32 :: Integer }
}


gzip_gemini_1 :: Daedalus GzipHeader
gzip_gemini_1 = GzipHeader { id = 1 }

gzip_gemini_2 :: Daedalus GzipTrailer
gzip_gemini_2 = GzipTrailer { crc32 = 0 }


--Example usage,  replace with your actual data and encoding/decoding logic.
main :: Daedalus ()
main = do
  h <- gzip_gemini_1
  t <- gzip_gemini_2
  return ()

