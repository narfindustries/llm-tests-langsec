enum ExtensionLabel : uint8
{
    GraphicControlExtension = 0xF9,
    CommentExtension = 0xFE,
    PlainTextExtension = 0x01,
    ApplicationExtension = 0xFF
}

type UInt16 = uint16;

struct Header
{
    magic : bstring(3) = "GIF";
    version : bstring(3) = "89a";
}

struct LogicalScreenDescriptor
{
    width : UInt16;
    height : UInt16;
    packedFields : PackedFields;
    backgroundColorIndex : uint8;
    pixelAspectRatio : uint8;
}

bitfield PackedFields : uint8
{
    globalColorTableFlag : bool @ 0 : 1;
    colorResolution : uint8 @ 1 : 3;
    sortFlag : bool @ 4 : 1;
    sizeOfGlobalColorTable : uint8 @ 5 : 3;
}

struct ColorTableEntry
{
    red : uint8;
    green : uint8;
    blue : uint8;
}

struct ImageDescriptor
{
    imageSeparator : uint8 = 0x2C;
    leftPosition : UInt16;
    topPosition : UInt16;
    width : UInt16;
    height : UInt16;
    packedFields : ImagePackedFields;
}

bitfield ImagePackedFields : uint8
{
    localColorTableFlag : bool @ 0 : 1;
    interlaceFlag : bool @ 1 : 1;
    sortFlag : bool @ 2 : 1;
    reserved : uint8 @ 3 : 2;
    sizeOfLocalColorTable : uint8 @ 5 : 3;
}

struct LZWImageData
{
    lzwMinimumCodeSize : uint8;
    data : DataSubBlocks;
}

struct DataSubBlock
{
    blockSize : uint8;
    data : bstring(blockSize);
}

struct DataSubBlocks
{
    blocks : sequence of DataSubBlock until ($last.blockSize == 0);
}

struct Trailer
{
    trailer : uint8 = 0x3B;
}

struct Extension
{
    extensionIntroducer : uint8 = 0x21;
    label : ExtensionLabel;
    blockSize : uint8;
    data : bstring(blockSize);
    subBlocks : DataSubBlocks;
}

struct GIFData
{
    extensionOrImage : uint8;
    switch (extensionOrImage)
    {
        case 0x21 : extension : Extension;
        case 0x2C : image : ImageDescriptorImageData;
    }
}

struct ImageDescriptorImageData
{
    descriptor : ImageDescriptor;
    imageData : LZWImageData;
}

struct GIFFile
{
    header : Header;
    logicalScreenDescriptor : LogicalScreenDescriptor;
    globalColorTable : sequence of ColorTableEntry if (logicalScreenDescriptor.packedFields.globalColorTableFlag);
    data : sequence of GIFData;
    trailer : Trailer;
}