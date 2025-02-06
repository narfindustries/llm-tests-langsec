grammar PNG;

import "std.daedalus";

struct PNGFile {
    u8[8] signature = [137, 80, 78, 71, 13, 10, 26, 10];
    Chunk[] chunks;
}

struct Chunk {
    u32 length;
    string chunkType : length=4;
    u8 data[length] @hidden;
    u32 crc;
    payload : switch (chunkType) {
        case "IHDR" => IHDRChunk(data);
        case "PLTE" => PLTEChunk(data);
        case "IDAT" => IDATChunk(data);
        case "IEND" => IENDChunk(data);
        case "tRNS" => tRNSChunk(data);
        case "gAMA" => gAMAChunk(data);
        case "cHRM" => cHRMChunk(data);
        case "sRGB" => sRGBChunk(data);
        case "iCCP" => iCCPChunk(data);
        case "tEXt" => tEXtChunk(data);
        case "zTXt" => zTXtChunk(data);
        case "iTXt" => iTXtChunk(data);
        case "bKGD" => bKGDChunk(data);
        case "hIST" => hISTChunk(data);
        case "pHYs" => pHYsChunk(data);
        case "sBIT" => sBITChunk(data);
        case "sPLT" => sPLTChunk(data);
        case "tIME" => tIMEChunk(data);
        default => UnknownChunk(data);
    }
}

struct IHDRChunk {
    u32 width;
    u32 height;
    u8 bitDepth;
    u8 colorType;
    u8 compressionMethod;
    u8 filterMethod;
    u8 interlaceMethod;
}

struct PLTEChunk {
    RGBEntry[] entries : length = _parent.length / 3;
}

struct RGBEntry {
    u8 red;
    u8 green;
    u8 blue;
}

struct IDATChunk {
    u8[] compressedData;
}

struct IENDChunk {
    // No fields, as IEND has no data
}

struct tRNSChunk {
    u8[] alphaValues;
}

struct gAMAChunk {
    u32 gamma;
}

struct cHRMChunk {
    u32 whitePointX;
    u32 whitePointY;
    u32 redX;
    u32 redY;
    u32 greenX;
    u32 greenY;
    u32 blueX;
    u32 blueY;
}

struct sRGBChunk {
    u8 renderingIntent;
}

struct iCCPChunk {
    string profileName;
    u8 compressionMethod;
    u8[] compressedProfile;
}

struct tEXtChunk {
    string keyword;
    string text;
}

struct zTXtChunk {
    string keyword;
    u8 compressionMethod;
    u8[] compressedText;
}

struct iTXtChunk {
    string keyword;
    u8 compressionFlag;
    u8 compressionMethod;
    string languageTag;
    string translatedKeyword;
    string text;
}

struct bKGDChunk {
    u8[] background;
}

struct hISTChunk {
    u16[] frequencies;
}

struct pHYsChunk {
    u32 pixelsPerUnitX;
    u32 pixelsPerUnitY;
    u8 unitSpecifier;
}

struct sBITChunk {
    u8[] significantBits;
}

struct sPLTChunk {
    string paletteName;
    u8 sampleDepth;
    SPaletteEntry[] entries : length = (_parent.length - paletteName.length - 2) / (2 + sampleDepth / 8 * 4);
}

struct SPaletteEntry {
    u16 red;
    u16 green;
    u16 blue;
    u16 alpha;
    u16 frequency;
}

struct tIMEChunk {
    u16 year;
    u8 month;
    u8 day;
    u8 hour;
    u8 minute;
    u8 second;
}

struct UnknownChunk {
    u8[] data;
}