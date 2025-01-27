-- TIFF (Tagged Image File Format) specification in Daedalus
-- This specification is designed to parse TIFF files, including their headers, IFDs, and tags.

import Daedalus

-- Define the basic types used in TIFF
def Byte   = UInt8
def Word   = UInt16
def DWord  = UInt32
def Offset = DWord

-- TIFF Header
def TIFFHeader =
  block
    byteOrder = ByteOrder
    magic     = Word
    ifdOffset = Offset

-- Byte Order (II for little-endian, MM for big-endian)
def ByteOrder =
  block
    order = UInt8
    case order of
      0x49 -> "II"  -- Little-endian
      0x4D -> "MM"  -- Big-endian

-- Image File Directory (IFD)
def IFD =
  block
    numEntries = Word
    entries    = Many numEntries IFDEntry
    nextIFD    = Offset

-- IFD Entry
def IFDEntry =
  block
    tag      = Word
    type     = Word
    count    = DWord
    value    = case type of
                1  -> Many count Byte   -- BYTE
                2  -> Many count Byte   -- ASCII
                3  -> Many count Word   -- SHORT
                4  -> Many count DWord  -- LONG
                5  -> Many count DWord  -- RATIONAL
                _  -> Fail "Unsupported IFD type"

-- Main TIFF structure
def TIFF =
  block
    header = TIFFHeader
    ifds   = ManyUntil (IFD) (nextIFD == 0)

-- Entry point
def Main = TIFF