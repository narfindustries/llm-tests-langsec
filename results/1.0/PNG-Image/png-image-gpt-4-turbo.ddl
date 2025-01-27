module PNGImage where

import Daedalus.Core
import Daedalus.Rec

-- Define the main structures based on the Portable Network Graphics (PNG) specification
type PNGFile = struct
  signature : Bytes 8
  chunks    : Array Chunk  until (isIENDChunk . value)

-- Check for the PNG signature at the beginning of the file
isPNGSignature : signature == [137, 80, 78, 71, 13, 10, 26, 10]

-- Define a PNG chunk
type Chunk = struct
  length : UInt32be
  typ    : Bytes 4
  value  : Bytes length    using ChunkData (decodeASCII typ)
  crc    : UInt32be

-- Check for the IEND chunk which marks the end of a PNG file
isIENDChunk : if typ == [73, 69, 78, 68] true else false

-- Decoding different types of PNG chunks by their type
type ChunkData (type : String) = switch type
  "IHDR" -> IHDRChunk
  "PLTE" -> PLTEChunk
  "IDAT" -> Bytes
  "IEND" -> Unit
  _      -> Bytes -- For other chunk types we don't specify detailed decoding

-- PNG image header chunk (IHDR)
type IHDRChunk = struct
  width            : UInt32be
  height           : UInt32be
  bitDepth         : UInt8
  colourType       : UInt8
  compressionMeth  : UInt8
  filterMethod     : UInt8
  interlaceMethod  : UInt8

-- Palette chunk (PLTE)
type PLTEChunk = struct
  paletteEntries   : Array Triple until end
type Triple = struct
  red   : UInt8
  green : UInt8
  blue  : UInt8

-- Parse method for a PNG file
def parsePNG : Unit
  image <- parse PNGFile
  match (isPNGSignature image.signature)
    false -> fail "Not a valid PNG file."
    true  -> return ()

-- Entry point for the parser
entry parsePNG