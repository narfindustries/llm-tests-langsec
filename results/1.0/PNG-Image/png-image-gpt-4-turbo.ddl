module PNG;

using Util::UInt32;
using Util::UInt8;

struct PNGFile {
    signature : UInt64 = 0x89504E470D0A1A0A;
    chunks : PNGChunk*;
}

alias CRC = UInt32;

struct PNGChunk {
    length : UInt32;
    type : String(4);
    data : select(this.type) {
        case "IHDR" => IHDRChunk;
        case "PLTE" => PLTEChunk;
        case "IDAT" => IDATChunk;
        case "IEND" => IENDChunk;
        case "tEXt" => tEXtChunk;
        case "zTXt" => zTXtChunk;
        case "iTXt" => iTXtChunk;
        case "bKGD" => bKGDChunk;
        case "pHYs" => pHYsChunk;
        case "tIME" => tIMEChunk;
        otherwise => Byte[this.length];
    };
    crc : CRC;
}

struct IHDRChunk {
    width : UInt32;
    height : UInt32;
    bitDepth : UInt8;
    colorType : UInt8;
    compressionMethod : UInt8;
    filterMethod : UInt8;
    interlaceMethod : UInt8;
}

struct PLTEChunk {
    entries : RGBTriple*;
}

struct RGBTriple {
    red : UInt8;
    green : UInt8;
    blue : UInt8;
}

struct IDATChunk {
    data : Byte*;
}

struct IENDChunk {
    // No fields in IEND Chunk
}

struct tEXtChunk {
    keyword : ZeroTerminatedString;
    text : UTF8String;
}

struct zTXtChunk {
    keyword : ZeroTerminatedString;
    compressionMethod : UInt8;
    compressedData : Byte*;
}

struct iTXtChunk {
    keyword : ZeroTerminatedString;
    compressionFlag : UInt8;
    compressionMethod : UInt8;
    languageTag : ZeroTerminatedString;
    translatedKeyword : ZeroTerminatedString;
    text : UTF8String;
}

struct bKGDChunk {
    backgroundColor : select(IHDRChunk.colorType) {
        case 0, 4 => UInt16;
        case 2, 6 => RGBTriple;
        case 3 => UInt8;
    };
}

struct pHYsChunk {
    pixelsPerUnitXAxis : UInt32;
    pixelsPerUnitYAxis : UInt32;
    unitSpecifier : UInt8;
}

struct tIMEChunk {
    year : UInt16;
    month : UInt8;
    day : UInt8;
    hour : UInt8;
    minute : UInt8;
    second : UInt8;
}