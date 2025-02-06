def LittleEndian = 0x4949
def BigEndian = 0x4D4D
def MagicNumber = 0x002A

enum Compression {
  NoCompression = 1
  CCITT3_1D = 2
  CCITT3_2D = 3
  CCITT4 = 4
  LZW = 5
  JPEG = 6
  PackBits = 32773
}

enum PhotometricInterpretation {
  WhiteIsZero = 0
  BlackIsZero = 1
  RGB = 2
  PaletteColor = 3
  TransparencyMask = 4
  CMYK = 5
  YCbCr = 6
}

enum Orientation {
  TopLeft = 1
  TopRight = 2
  BottomRight = 3
  BottomLeft = 4
  LeftTop = 5
  RightTop = 6
  RightBottom = 7
  LeftBottom = 8
}

enum ResolutionUnit {
  NoUnit = 1
  Inch = 2
  Centimeter = 3
}

enum Tag {
  NewSubfileType = 254
  SubfileType = 255
  ImageWidth = 256
  ImageLength = 257
  BitsPerSample = 258
  Compression = 259
  PhotometricInterpretation = 260
  Thresholding = 261
  CellWidth = 262
  CellLength = 263
  FillOrder = 264
  DocumentName = 265
  ImageDescription = 266
  Make = 267
  Model = 268
  StripOffsets = 269
  Orientation = 270
  SamplesPerPixel = 271
  RowsPerStrip = 272
  StripByteCounts = 273
  MinSampleValue = 274
  MaxSampleValue = 275
  XResolution = 276
  YResolution = 277
  PlanarConfiguration = 278
  ResolutionUnit = 279
  PageName = 280
  XPosition = 281
  YPosition = 282
  Software = 283
  DateTime = 284
  Artist = 285
  HostComputer = 286
  Predictor = 287
  WhitePoint = 288
  PrimaryChromaticities = 289
}

enum DataType {
  Byte = 1
  ASCII = 2
  Short = 3
  Long = 4
  Rational = 5
}

struct IFDEntry {
  tag: Tag
  dataType: DataType
  count: u32
  valueOrOffset: u32
}

struct TIFFHeader {
  byteOrder: u16
  magicNumber: u16
  firstIFDOffset: u32
}

struct TIFFFile {
  header: TIFFHeader
  imageFileDirectories: List<{
    numEntries: u16
    entries: List<IFDEntry>
    nextIFDOffset: Option<u32>
  }>
}