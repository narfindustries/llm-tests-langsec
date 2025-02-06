enum Compression : uint16 {
    NoCompression = 1,
    CCITT_1D = 2,
    Group3Fax = 3,
    Group4Fax = 4,
    LZW = 5,
    JPEG = 6, // Deprecated
    PackBits = 32773
}

enum PhotometricInterpretation : uint16 {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    PaletteColor = 3,
    TransparencyMask = 4,
    CMYK = 5,
    YCbCr = 6,
    CIELab = 8
}

enum ResolutionUnit : uint16 {
    None = 1,
    Inch = 2,
    Centimeter = 3
}

enum PlanarConfiguration : uint16 {
    Chunky = 1,
    Planar = 2
}

enum FillOrder : uint16 {
    MSB2LSB = 1,
    LSB2MSB = 2
}

enum Orientation : uint16 {
    TopLeft = 1,
    TopRight = 2,
    BottomRight = 3,
    BottomLeft = 4,
    LeftTop = 5,
    RightTop = 6,
    RightBottom = 7,
    LeftBottom = 8
}

struct Rational {
    uint32 numerator,
    uint32 denominator
}

struct IFDEntry {
    uint16 tag,
    uint16 type,
    uint32 count,
    uint32 value_offset
}

struct TIFFHeader {
    uint16 byte_order,
    uint16 version,
    uint32 ifd_offset
}

struct TIFF {
    TIFFHeader header,
    IFDEntry[] ifd_entries,

    // Optional fields
    uint32 ImageWidth if exists(ifd_entries, 256),
    uint32 ImageLength if exists(ifd_entries, 257),
    uint16[] BitsPerSample if exists(ifd_entries, 258),
    Compression compression if exists(ifd_entries, 259),
    PhotometricInterpretation photometric_interpretation if exists(ifd_entries, 262),
    uint32[] StripOffsets if exists(ifd_entries, 273),
    uint16 SamplesPerPixel if exists(ifd_entries, 277),
    uint32 RowsPerStrip if exists(ifd_entries, 278),
    uint32[] StripByteCounts if exists(ifd_entries, 279),
    Rational XResolution if exists(ifd_entries, 282),
    Rational YResolution if exists(ifd_entries, 283),
    ResolutionUnit resolution_unit if exists(ifd_entries, 296),
    PlanarConfiguration planar_configuration if exists(ifd_entries, 284),
    FillOrder fill_order if exists(ifd_entries, 266),
    Orientation orientation if exists(ifd_entries, 274),
    uint16[] ColorMap if exists(ifd_entries, 320),
    uint32 TileWidth if exists(ifd_entries, 322),
    uint32 TileLength if exists(ifd_entries, 323),
    uint32[] TileOffsets if exists(ifd_entries, 324),
    uint32[] TileByteCounts if exists(ifd_entries, 325)
}