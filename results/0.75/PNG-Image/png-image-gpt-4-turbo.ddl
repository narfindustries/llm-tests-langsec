-- Daedalus specification for a simple PNG file format.

module PNGImage where

import DAEDALUS.Core

-- Define the PNG file signature.
pngSignature : Bitstream = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- Define the chunk structure.
data Chunk = Chunk {
  length  : UInt32,
  typ     : UInt32,
  data    : Vector (UInt 8) (value length),
  crc     : UInt32
}

-- Parse a single chunk.
chunk : Parser Chunk = 
  do len    <- uint32be
     typ    <- uint32be
     dat    <- vec (uint8) (fromIntegral len)
     crcVal <- uint32be
     return Chunk { length = len, typ = typ, data = dat, crc = crcVal }

-- Parse multiple chunks until IEND.
chunks : Parser (Vector Chunk) = manyTill chunk (lookAhead (bytes (uint32be == 0x49454E44)))

-- Define the PNG data structure.
data PNGFile = PNGFile {
  header  : Bitstream,
  chunks  : Vector Chunk
}

-- Parse the PNG file structure.
pngFile : Parser PNGFile =
  do hdr    <- bytes (pngSignature)
     chks   <- chunks
     return PNGFile { header = hdr, chunks = chks }

-- Main function to parse a PNG file.
main : Parser PNGFile = pngFile