enum Version : u24 {
    GIF87a = 0x383761,
    GIF89a = 0x383961
}

enum ExtensionLabel : u8 {
    GraphicControl = 0xF9,
    Comment = 0xFE,
    PlainText = 0x01,
    Application = 0xFF
}

struct Header {
    signature : u24 = 0x474946;
    version : Version;
}

struct LogicalScreenDescriptor {
    width : u16;
    height : u16;
    packedFields : u8;
    backgroundColorIndex : u8;
    pixelAspectRatio : u8;
}

struct ColorTableEntry {
    red : u8;
    green : u8;
    blue : u8;
}

struct ImageDescriptor {
    imageSeparator : u8 = 0x2C;
    leftPosition : u16;
    topPosition : u16;
    width : u16;
    height : u16;
    packedFields : u8;
}

struct ImageData {
    lzwMinimumCodeSize : u8;
    dataBlocks : DataBlock[];
}

struct DataBlock {
    size : u8;
    data : u8[size];
}

struct Trailer {
    trailer : u8 = 0x3B;
}

struct Extension {
    introducer : u8 = 0x21;
    label : ExtensionLabel;
    blockSize : u8;
    data : u8[blockSize];
    terminator : u8 = 0x00;
}

struct GIF {
    header : Header;
    logicalScreenDescriptor : LogicalScreenDescriptor;
    globalColorTable : ColorTableEntry[2 ** ((logicalScreenDescriptor.packedFields & 0x07) + 1)] if (logicalScreenDescriptor.packedFields & 0x80) != 0;
    blocks : (ImageDescriptor & ImageData | Extension)*;
    trailer : Trailer;
}