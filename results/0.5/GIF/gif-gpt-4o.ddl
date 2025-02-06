enum ExtensionLabel : u8 {
    PlainText = 0x01,
    GraphicControl = 0xF9,
    Comment = 0xFE,
    Application = 0xFF
}

struct Header {
    signature : u8[3] = "GIF";
    version : u8[3] = "89a";
}

struct LogicalScreenDescriptor {
    width : u16le;
    height : u16le;
    packedFields : u8;
    backgroundColorIndex : u8;
    pixelAspectRatio : u8;
    
    globalColorTableFlag : bool { (packedFields & 0x80) != 0 }
    colorResolution : u8 { (packedFields >> 4) & 0x07 }
    sortFlag : bool { (packedFields & 0x08) != 0 }
    sizeOfGlobalColorTable : u8 { packedFields & 0x07 }
}

struct ColorTableEntry {
    red : u8;
    green : u8;
    blue : u8;
}

struct ImageDescriptor {
    separator : u8 = 0x2C;
    leftPosition : u16le;
    topPosition : u16le;
    width : u16le;
    height : u16le;
    packedFields : u8;
    
    localColorTableFlag : bool { (packedFields & 0x80) != 0 }
    interlaceFlag : bool { (packedFields & 0x40) != 0 }
    sortFlag : bool { (packedFields & 0x20) != 0 }
    sizeOfLocalColorTable : u8 { packedFields & 0x07 }
}

struct ImageDataBlock {
    blockSize : u8;
    data : u8[blockSize];
}

struct ImageData {
    lzwMinimumCodeSize : u8;
    dataBlocks : ImageDataBlock[] : until(dataBlocks[-1].blockSize == 0);
}

struct Trailer {
    trailer : u8 = 0x3B;
}

struct Extension {
    introducer : u8 = 0x21;
    label : ExtensionLabel;
    blockSize : u8;
    data : u8[blockSize];
    blockTerminator : u8 = 0x00;
}

struct GifFile {
    header : Header;
    logicalScreenDescriptor : LogicalScreenDescriptor;
    globalColorTable : ColorTableEntry[1 << (logicalScreenDescriptor.sizeOfGlobalColorTable + 1)] : if(logicalScreenDescriptor.globalColorTableFlag);
    blocks : (ImageDescriptor | Extension | Trailer)[];
}