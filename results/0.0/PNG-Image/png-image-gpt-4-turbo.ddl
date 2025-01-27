module PNGImage where

import DAEDALUS.Core

-- Define the PNG file structure
data PNGFile = PNGFile {
  header      :: IHDRChunk,
  chunks      :: [Chunk]
}

-- Define the IHDR chunk structure
data IHDRChunk = IHDRChunk {
  width       :: UInt32,
  height      :: UInt32,
  bitDepth    :: UInt8,
  colorType   :: UInt8,
  compression :: UInt8,
  filter      :: UInt8,
  interlace   :: UInt8
}

-- Define the generic chunk structure
data Chunk = Chunk {
  chunkType   :: String,
  chunkData   :: Bytes,
  crc         :: UInt32
}

-- Parse the PNG header
parseHeader :: Parser IHDRChunk
parseHeader = do
  string "IHDR"
  width       <- uint32
  height      <- uint32
  bitDepth    <- uint8
  colorType   <- uint8
  compression <- uint8
  filter      <- uint8
  interlace   <- uint8
  return IHDRChunk { width, height, bitDepth, colorType, compression, filter, interlace }

-- Parse a generic chunk
parseChunk :: Parser Chunk
parseChunk = do
  chunkType <- stringN 4
  len       <- uint32
  chunkData <- bytes len
  crc       <- uint32
  return Chunk { chunkType, chunkData, crc }

-- Parse the entire PNG file
parsePNGFile :: Parser PNGFile
parsePNGFile = do
  string "\x89PNG\r\n\x1a\n"
  header <- parseHeader
  chunks <- many parseChunk
  return PNGFile { header, chunks }

-- Entry point for parsing
pngParser :: Parser PNGFile
pngParser = parsePNGFile