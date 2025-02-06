module GIF89a;

import std.bits;
import std.seq;

type Color = struct {
    Red   : U8;
    Green : U8;
    Blue  : U8;
};

type LogicalScreenDescriptor = struct {
    ScreenWidth        : U16;
    ScreenHeight       : U16;
    PackedFields       : U8;
    BackgroundColorIndex : U8;
    PixelAspectRatio   : U8;

    GlobalColorTableFlag() : Bool = (PackedFields >> 7) & 1 != 0;
    ColorResolution() : U8 = (PackedFields >> 4) & 0b111;
    SortFlag() : Bool = ((PackedFields >> 3) & 1) != 0;
    SizeOfGlobalColorTable() : U8 = PackedFields & 0b111;
};

type GlobalColorTable = array<Color>[|2^(parent.LogicalScreenDescriptor.SizeOfGlobalColorTable() + 1)|];

type ImageDescriptor = struct {
    ImageLeftPosition : U16;
    ImageTopPosition  : U16;
    ImageWidth        : U16;
    ImageHeight       : U16;
    PackedFields      : U8;

    LocalColorTableFlag() : Bool = (PackedFields >> 7) & 1 != 0;
    InterlaceFlag() : Bool = (PackedFields >> 6) & 1 != 0;
    SortFlag() : Bool = (PackedFields >> 5) & 1 != 0;
    SizeOfLocalColorTable() : U8 = PackedFields & 0b111;
};

type LocalColorTable = array<Color>[|2^(parent.ImageDescriptor.SizeOfLocalColorTable() + 1)|];

type ImageData = struct {
    LZWMinimumCodeSize : U8;
    DataBlocks         : array<struct {BlockSize: U8; Data: bytes[BlockSize];}>[|until (DataBlocks[0].BlockSize == 0)|];
};

type GraphicControlExtension = struct {
    BlockSize         : U8;
    PackedFields      : U8;
    DelayTime         : U16;
    TransparentColorIndex : U8;
    BlockTerminator   : U8;
};

type CommentExtension = struct {
    CommentData : array<struct {BlockSize: U8; Data: bytes[BlockSize];}>[|until (CommentData[0].BlockSize == 0)|];
};

type PlainTextExtension = struct {
    BlockSize          : U8;
    TextGridLeftPosition : U16;
    TextGridTopPosition  : U16;
    TextGridWidth      : U16;
    TextGridHeight     : U16;
    CharacterCellWidth : U8;
    CharacterCellHeight: U8;
    TextForegroundColorIndex : U8;
    TextBackgroundColorIndex : U8;
    PlainTextData      : array<struct {BlockSize: U8; Data: bytes[BlockSize];}>[|until (PlainTextData[0].BlockSize == 0)|];
};

type ApplicationExtension = struct {
    BlockSize         : U8;
    ApplicationIdentifier : bytes[8];
    ApplicationAuthenticationCode : bytes[3];
    ApplicationData   : array<struct {BlockSize: U8; Data: bytes[BlockSize];}>[|until (ApplicationData[0].BlockSize == 0)|];
};

type Extension = struct {
    Introducer : U8; // Always 0x21
    Label      : U8;
    Content    : switch (Label) {
        case 0xF9 => GraphicControlExtension;
        case 0xFE => CommentExtension;
        case 0x01 => PlainTextExtension;
        case 0xFF => ApplicationExtension;
    };
};

type Block = switch (peek U8) {
    case 0x2C => ImageDescriptor;
    case 0x21 => Extension;
};

type GIFFile = struct {
    Header                : bytes[6];
    LogicalScreenDescriptor : LogicalScreenDescriptor;
    GlobalColorTable      : GlobalColorTable if LogicalScreenDescriptor.GlobalColorTableFlag();
    Blocks                : array<Block>[|until (peek U8 == 0x3B)|];
    Trailer               : U8; // Always 0x3B
};

stream ParseGIF(input : bytes) : GIFFile {
    return parse<GIFFile>(input);
}