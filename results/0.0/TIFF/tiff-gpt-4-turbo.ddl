module TIFF.GPT4Turbo {
  import TIFF.Common;

  type IFD = struct {
    numEntries : uint16;
    entries    : Entry[numEntries];
    nextIFD    : uint32;
  }

  type Entry = struct {
    tag        : uint16;
    type       : uint16;
    count      : uint32;
    value      : value(type, count);
  }

  type value = fn (type: uint16, count: uint32) -> dynamic {
    switch type {
      case 1  => uint8[count];   // BYTE
      case 2  => uint8[count];   // ASCII
      case 3  => uint16[count];  // SHORT
      case 4  => uint32[count];  // LONG
      case 5  => Rational[count]; // RATIONAL
      case 6  => int8[count];    // SBYTE
      case 7  => uint8[count];   // UNDEFINED
      case 8  => int16[count];   // SSHORT
      case 9  => int32[count];   // SLONG
      case 10 => SRational[count]; // SRATIONAL
      case 11 => float32[count]; // FLOAT
      case 12 => float64[count]; // DOUBLE
      default => uint8[count];   // Unknown type, default to array of bytes
    }
  }

  type Rational = struct {
    numerator   : uint32;
    denominator : uint32;
  }

  type SRational = struct {
    numerator   : int32;
    denominator : int32;
  }

  type TIFFFile = struct {
    endian      : uint16;
    magic       : uint16;
    ifdOffset   : uint32;
    ifds        : IFD[1] @offset(ifdOffset);
  }

  type Main = struct {
    header : TIFFFile;
  }
}