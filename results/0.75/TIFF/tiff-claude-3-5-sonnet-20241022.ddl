type Header = {
  byteOrder: u16 where byteOrder == 0x4949 || byteOrder == 0x4D4D,
  version: u16 where version == 42,
  firstIFDOffset: u32
}

type TagType = u16 union {
  BYTE = 1,
  ASCII = 2,
  SHORT = 3,
  LONG = 4,
  RATIONAL = 5,
  SBYTE = 6,
  UNDEFINED = 7,
  SSHORT = 8,
  SLONG = 9,
  SRATIONAL = 10,
  FLOAT = 11,
  DOUBLE = 12
}

type Compression = u16 union {
  NONE = 1,
  CCITT_GROUP3_1D = 2,
  CCITT_GROUP3_FAX = 3,
  CCITT_GROUP4_FAX = 4,
  LZW = 5,
  JPEG = 6,
  PACKBITS = 32773
}

type PhotometricInterpretation = u16 union {
  WHITE_IS_ZERO = 0,
  BLACK_IS_ZERO = 1,
  RGB = 2,
  PALETTE_COLOR = 3,
  TRANSPARENCY_MASK = 4,
  CMYK = 5,
  YCBCR = 6,
  CIELAB = 8
}

type ResolutionUnit = u16 union {
  NO_UNIT = 1,
  INCH = 2,
  CENTIMETER = 3
}

type PlanarConfiguration = u16 union {
  CHUNKY = 1,
  PLANAR = 2
}

type Tag = u16 union {
  IMAGE_WIDTH = 256,
  IMAGE_LENGTH = 257,
  BITS_PER_SAMPLE = 258,
  COMPRESSION = 259,
  PHOTOMETRIC_INTERPRETATION = 262,
  STRIP_OFFSETS = 273,
  SAMPLES_PER_PIXEL = 277,
  ROWS_PER_STRIP = 278,
  STRIP_BYTE_COUNTS = 279,
  X_RESOLUTION = 282,
  Y_RESOLUTION = 283,
  PLANAR_CONFIGURATION = 284,
  RESOLUTION_UNIT = 296,
  COLOR_MAP = 320,
  DATE_TIME = 306,
  ARTIST = 315,
  SOFTWARE = 305,
  MAKE = 271,
  MODEL = 272,
  COPYRIGHT = 33432
}

type Rational = {
  numerator: u32,
  denominator: u32
}

type SRational = {
  numerator: i32,
  denominator: i32
}

type IFDEntry = {
  tag: Tag,
  type: TagType,
  count: u32,
  valueOffset: u32
}

type IFD = {
  numEntries: u16,
  entries: IFDEntry[numEntries],
  nextIFDOffset: u32
}

type TIFF = {
  header: Header,
  ifd: IFD
}

let Main = TIFF