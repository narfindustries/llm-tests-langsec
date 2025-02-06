enum Compression : uint16 {
    None = 1;
    CCITT_1D = 2;
    Group_3_Fax = 3;
    Group_4_Fax = 4;
    LZW = 5;
    JPEG = 6;
    PackBits = 32773;
}

enum PhotometricInterpretation : uint16 {
    WhiteIsZero = 0;
    BlackIsZero = 1;
    RGB = 2;
    Palette = 3;
    TransparencyMask = 4;
    CMYK = 5;
    YCbCr = 6;
    CIELab = 8;
}

enum ResolutionUnit : uint16 {
    None = 1;
    Inch = 2;
    Centimeter = 3;
}

enum PlanarConfiguration : uint16 {
    Chunky = 1;
    Planar = 2;
}

enum SampleFormat : uint16 {
    UnsignedInt = 1;
    SignedInt = 2;
    IEEEFloat = 3;
    Undefined = 4;
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
    uint16 version; // 42 for TIFF
    uint32 ifdOffset;
}

struct IFD {
    uint16 numEntries;
    TIFFTag[numEntries] entries;
    uint32 nextIFDOffset;
}

struct TIFF {
    TIFFHeader header;
    IFD ifd;

    // Optional fields
    optional uint32 ImageWidth;
    optional uint32 ImageLength;
    optional uint16 BitsPerSample;
    optional Compression compression;
    optional PhotometricInterpretation photometricInterpretation;
    optional uint32 StripOffsets;
    optional uint16 SamplesPerPixel;
    optional uint32 RowsPerStrip;
    optional uint32 StripByteCounts;
    optional Rational XResolution;
    optional Rational YResolution;
    optional ResolutionUnit resolutionUnit;
    optional PlanarConfiguration planarConfiguration;
    optional uint16 ColorMap;
    optional uint32 TileWidth;
    optional uint32 TileLength;
    optional uint32 TileOffsets;
    optional uint32 TileByteCounts;
    optional uint16 ExtraSamples;
    optional SampleFormat sampleFormat;
}