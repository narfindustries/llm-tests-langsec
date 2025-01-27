module TIFF.GPT4Turbo {

  import std::bits
  import std::bytes

  -- Data Type Definitions
  type UInt8   = bits.UInt8
  type UInt16  = bits.UInt16
  type UInt32  = bits.UInt32
  type UInt64  = bits.UInt64

  type Byte    = bytes.Byte

  -- Main image file directory (IFD) entry
  type IFDEntry = struct {
    tag    : UInt16
    type   : UInt16
    count  : UInt32
    offset : choice (UInt32) { 
      asBytes   : (count <= 4) -> bytes.ByteString(count)
      asAddress : *Byte 
    }
  }

  -- TIFF Header
  type TIFFHeader = struct {
    -- Byte order marks indicating little or big endian
    byteOrder    : fixed bytes.Bytes(2)
    fortyTwo     : UInt16   -- Always 42
    ifdOffset    : choice (UInt32) {
      direct      : 0x00000008 -> IFDSection
      offset      : *IFDSection
    }
  }

  type IFDSection = struct {
    numEntries : UInt16
    entries    : repeat IFDEntry(numEntries)
    nextIFD    : choice (UInt32) {
      singleIFD  : 0 -> ()
      nextOffset : *IFDSection
    }
  }

  -- Support choosing endianess based on header field
  type EndianWrapper = struct {
    header: choice TIFFHeader {
      useBigEndian    : (header.byteOrder == bytes.Bytes2(0x4D, 0x4D)) -> big TIFFHeader
      useLittleEndian : (header.byteOrder == bytes.Bytes2(0x49, 0x49)) -> little TIFFHeader
    }
  }

  -- Entry point
  type TIFFImage = struct {
    wrappedHeader: EndianWrapper
    body         : *wrappedHeader.header.ifdOffset  -- based on choice in header
  }
}