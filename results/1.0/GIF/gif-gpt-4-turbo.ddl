module GIF;

import std.binary;

struct RGB {
    u8 red;
    u8 green;
    u8 blue;
}

struct LogicalScreenDescriptor {
    u16 screenWidth;
    u16 screenHeight;
    u8 packedFields;
    u8 backgroundColorIndex;
    u8 pixelAspectRatio;

    bit globalColorTableFlag = packedFields[7];
    u32 colorResolution = (packedFields >> 4) & 0x07 + 1;
    bit sortFlag = packedFields[3];
    u32 sizeOfGlobalColorTable = 1 << ((packedFields & 0x07) + 1);
}

struct GlobalColorTable {
    RGB colors[depends_on size];
}

struct ImageDescriptor {
    u8 imageSeparator;
    u16 imageLeftPosition;
    u16 imageTopPosition;
    u16 imageWidth;
    u16 imageHeight;
    u8 packedFields;

    bit localColorTableFlag = packedFields[7];
    bit interlaceFlag = packedFields[6];
    bit sortFlag = packedFields[5];
    u32 sizeOfLocalColorTable = 1 << ((packedFields & 0x07) + 1);
}

struct LocalColorTable {
    RGB colors[depends_on size];
}

struct ImageData {
    ImageDescriptor descriptor;
    Optional<LocalColorTable> localColorTable[descriptor.localColorTableFlag];
    u8 lzwMinimumCodeSize;
    [u8] imageData;
}

struct ExtensionBlock {
    u8 extensionIntroducer; // Should be 0x21
    u8 extensionLabel;
}

struct GraphicControlExtension {
    ExtensionBlock blockStart;
    u8 blockSize;
    u8 packedFields;
    u16 delayTime;
    u8 transparentColorIndex;
    u8 blockTerminator;
}

struct CommentExtension {
    ExtensionBlock blockStart;
    u8 blockSize;
    [u8] commentData;
}

struct PlainTextExtension {
    ExtensionBlock blockStart;
    u8 blockSize;
    u16 textGridLeftPosition;
    u16 textGridTopPosition;
    u16 textGridWidth;
    u16 textGridHeight;
    u8 characterCellWidth;
    u8 characterCellHeight;
    u8 textForegroundColorIndex;
    u8 textBackgroundColorIndex;
    [u8] plainTextData;
}

struct ApplicationExtension {
    ExtensionBlock blockStart;
    u8 blockSize;
    string(8) applicationIdentifier;
    string(3) applicationAuthCode;
    [u8] applicationData;
}

struct GIFFile {
    string(3) signature;
    string(3) version;
    LogicalScreenDescriptor logicalScreenDescriptor;
    Optional<GlobalColorTable> globalColorTable[logicalScreenDescriptor.globalColorTableFlag];
    Choice<ImageData, GraphicControlExtension, CommentExtension, PlainTextExtension, ApplicationExtension>* block;
    u8 trailer; // Should be 0x3B
}