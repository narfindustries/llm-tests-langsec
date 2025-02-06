enum ColorType : uint8 {
    Grayscale = 0;
    Truecolor = 2;
    IndexedColor = 3;
    GrayscaleWithAlpha = 4;
    TruecolorWithAlpha = 6;
};

enum CompressionMethod : uint8 {
    Deflate = 0;
};

enum FilterMethod : uint8 {
    Adaptive = 0;
};

enum InterlaceMethod : uint8 {
    None = 0;
    Adam7 = 1;
};

struct PNGSignature {
    uint8 signature[8] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
};

struct Chunk {
    uint32 length;
    char type[4];
    uint8 data[length];
    uint32 crc;
};

struct IHDR {
    uint32 width;
    uint32 height;
    uint8 bitDepth;
    ColorType colorType;
    CompressionMethod compressionMethod;
    FilterMethod filterMethod;
    InterlaceMethod interlaceMethod;
};

struct PLTE {
    uint8 paletteEntries[];
};

struct IDAT {
    uint8 imageData[];
};

struct IEND {};

struct tRNS {
    uint8 transparencyData[];
};

struct cHRM {
    uint32 whitePointX;
    uint32 whitePointY;
    uint32 redX;
    uint32 redY;
    uint32 greenX;
    uint32 greenY;
    uint32 blueX;
    uint32 blueY;
};

struct gAMA {
    uint32 gamma;
};

struct iCCP {
    char profileName[];
    uint8 compressionMethod;
    uint8 compressedProfile[];
};

struct sBIT {
    uint8 significantBits[];
};

struct sRGB {
    uint8 renderingIntent;
};

struct tEXt {
    char keyword[];
    char text[];
};

struct zTXt {
    char keyword[];
    uint8 compressionMethod;
    uint8 compressedText[];
};

struct iTXt {
    uint8 compressionFlag;
    uint8 compressionMethod;
    char languageTag[];
    char translatedKeyword[];
    char text[];
};

struct bKGD {
    uint8 backgroundColor[];
};

struct pHYs {
    uint32 pixelsPerUnitX;
    uint32 pixelsPerUnitY;
    uint8 unitSpecifier;
};

struct sPLT {
    char paletteName[];
    uint8 sampleDepth;
    uint8 paletteEntries[];
};

struct hIST {
    uint16 frequency[];
};

struct tIME {
    uint16 year;
    uint8 month;
    uint8 day;
    uint8 hour;
    uint8 minute;
    uint8 second;
};

struct PNG {
    PNGSignature signature;
    IHDR ihdr;
    PLTE? plte;
    IDAT idat[];
    IEND iend;
    tRNS? trns;
    cHRM? chrm;
    gAMA? gama;
    iCCP? iccp;
    sBIT? sbit;
    sRGB? srgb;
    tEXt? text[];
    zTXt? ztext[];
    iTXt? itext[];
    bKGD? bkgd;
    pHYs? phys;
    sPLT? splt[];
    hIST? hist;
    tIME? time;
};