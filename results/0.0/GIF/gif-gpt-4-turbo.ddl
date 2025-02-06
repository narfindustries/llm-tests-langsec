module GIF89a;

import std::ascii;
import std::bits;
import std::bytes;

type Color = struct {
    red   : u8;
    green : u8;
    blue  : u8;
};

type LogicalScreenDescriptor = struct {
    screenWidth        : u16;
    screenHeight       : u16;
    packedFields       : u8;
    backgroundColorIdx : u8;
    pixelAspectRatio   : u8;

    globalColorTableFlag() : bool = (packedFields >> 7) & 1 != 0;
    colorResolution() : u8 = ((packedFields >> 4) & 0x07) + 1;
    sortFlag() : bool = (packedFields >> 3) & 1 != 0;
    globalColorTableSize() : u32 = 1 << ((packedFields & 0x07) + 1);
};

type GlobalColorTable = struct {
    colors : Color[globalColorTableSize()];
};

type ImageDescriptor = struct {
    imageLeftPosition   : u16;
    imageTopPosition    : u16;
    imageWidth          : u16;
    imageHeight         : u16;
    packedFields        : u8;

    localColorTableFlag() : bool = (packedFields >> 7) & 1 != 0;
    interlaceFlag() : bool = (packedFields >> 6) & 1 != 0;
    sortFlag() : bool = (packedFields >> 5) & 1 != 0;
    localColorTableSize() : u32 = 1 << ((packedFields & 0x07) + 1);
};

type LocalColorTable = struct {
    colors : Color[localColorTableSize()];
};

type ImageData = struct {
    lzwMinimumCodeSize : u8;
    dataBlocks         : bytes;
};

type GraphicControlExtension = struct {
    blockSize          : u8;
    packedFields       : u8;
    delayTime          : u16;
    transparentColorIdx: u8;
    terminator         : u8;
};

type CommentExtension = struct {
    commentData : bytes;
};

type PlainTextExtension = struct {
    blockSize             : u8;
    textGridLeftPosition  : u16;
    textGridTopPosition   : u16;
    textGridWidth         : u16;
    textGridHeight        : u16;
    characterWidth        : u8;
    characterHeight       : u8;
    textColorIndex        : u8;
    backgroundColorIndex  : u8;
    textData              : bytes;
};

type ApplicationExtension = struct {
    blockSize             : u8;
    applicationIdentifier : ascii[8];
    applicationAuthCode   : bytes[3];
    applicationData       : bytes;
};

type Block = union {
    imageDescriptor        : ImageDescriptor;
    graphicControlExtension: GraphicControlExtension;
    commentExtension       : CommentExtension;
    plainTextExtension     : PlainTextExtension;
    applicationExtension   : ApplicationExtension;
};

type GIF = struct {
    header                : ascii[6];
    logicalScreenDescriptor: LogicalScreenDescriptor;
    globalColorTable      : GlobalColorTable[logicalScreenDescriptor.globalColorTableFlag()];
    blocks                : Block[];
    trailer               : u8;
};