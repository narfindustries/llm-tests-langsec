enum uint16 Compression {
    NoCompression = 1,
    CCITTGroup3 = 2,
    CCITTT4 = 3,
    CCITTT6 = 4,
    LZW = 5,
    JPEG = 6,
    PackBits = 32773
}

enum uint16 PhotometricInterpretation {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    PaletteColor = 3,
    TransparencyMask = 4,
    CMYK = 5,
    YCbCr = 6,
    CIELab = 8
}

enum uint16 ResolutionUnit {
    None = 1,
    Inch = 2,
    Centimeter = 3
}

enum uint16 PlanarConfiguration {
    Chunky = 1,
    Planar = 2
}

enum uint8 ExtraSamples {
    Unspecified = 0,
    AssociatedAlpha = 1,
    UnassociatedAlpha = 2
}

enum uint16 SampleFormat {
    UnsignedInteger = 1,
    SignedInteger = 2,
    IEEEFloat = 3,
    Undefined = 4
}

struct Rational {
    uint32 numerator;
    uint32 denominator;
}

struct TIFFTag {
    uint16 tag;
    uint16 type;
    uint32 count;
    uint32 valueOffset;
}

struct TIFFHeader {
    uint16 byteOrder; // 0x4949 for little-endian, 0x4D4D for big-endian
    uint16 version; // 42
    uint32 ifdOffset;
}

struct IFDEntry {
    uint16 tag;
    uint16 type;
    uint32 count;
    uint32 valueOffset;
}

struct IFD {
    uint16 numEntries;
    IFDEntry[numEntries] entries;
    uint32 nextIFDOffset;
}

struct TIFF {
    TIFFHeader header;
    IFD ifd;
}