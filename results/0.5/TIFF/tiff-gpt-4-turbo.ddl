module TIFF.GPT4Turbo {

  import DAEDALUS::BitStream;

  type Byte = UInt8;

  type Word = UInt16 {
    littleEndian
  };

  type DWord = UInt32 {
    littleEndian
  };

  type Long = UInt32 {
    littleEndian
  };

  type Rational = struct {
    numerator : Long;
    denominator : Long;
  };

  type TIFFHeader = struct {
    endianess : Word;     // 0x4949 (little endian) or 0x4D4D (big endian)
    magic : Word;         // Fixed value: 42
    ifdOffset : Long;     // Offset to the first Image File Directory (IFD)
  };

  type IFDEntry = struct {
    tag : Word;
    type : Word;
    count : Long;
    valueOffset : Long;
  };

  type IFD = struct {
    numEntries : Word;
    entries : IFDEntry[numEntries];
    nextIFDOffset : Long;
  };

  type ImageData = struct {
    data : Byte[];
  };

  type TIFFFile = struct {
    header : TIFFHeader;
    firstIFD : IFD @ header.ifdOffset;
    image : ImageData @ firstIFD.entries[1].valueOffset; // Assuming the second entry points to image data
  };

  // Entry point for parsing
  let parse_tiff = parse TIFFFile from BitStream;
}