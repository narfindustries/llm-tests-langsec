module GIF.GPT4Turbo {
  import DAEDALUS::Core;
  import DAEDALUS::Bit;

  type U8 = UInt(8);
  type U16 = UInt(16);
  type U32 = UInt(32);

  type LogicalScreenDescriptor = struct {
    width           : U16;
    height          : U16;
    packedFields    : U8;
    backgroundColor : U8;
    pixelAspectRatio: U8;
  };

  type ColorTableEntry = struct {
    red   : U8;
    green : U8;
    blue  : U8;
  };

  type GlobalColorTable = struct {
    entries : ColorTableEntry[|this.numEntries|];
  } where {
    numEntries : U32 = (1 << ((packedFields & 0x07) + 1));
  };

  type ImageDescriptor = struct {
    leftPosition   : U16;
    topPosition    : U16;
    width          : U16;
    height         : U16;
    packedFields   : U8;
  };

  type LocalColorTable = struct {
    entries : ColorTableEntry[|this.numEntries|];
  } where {
    numEntries : U32 = (1 << ((packedFields & 0x07) + 1));
  };

  type TableBasedImageData = struct {
    lzwMinimumCodeSize : U8;
    imageDataBlocks    : Bytes;
  };

  type GraphicControlExtension = struct {
    blockSize        : U8;
    packedFields     : U8;
    delayTime        : U16;
    transparentColorIndex : U8;
    terminator       : U8;
  };

  type ApplicationExtension = struct {
    blockSize    : U8;
    applicationIdentifier : String(8);
    applicationAuthCode   : Bytes(3);
    applicationData       : Bytes;
  };

  type CommentExtension = struct {
    commentData : String;
  };

  type Block = union {
    case 0x2C : ImageDescriptor;
    case 0x21 : Extension;
    case 0x3B : End;
  };

  type Extension = union {
    case 0xF9 : GraphicControlExtension;
    case 0xFE : CommentExtension;
    case 0xFF : ApplicationExtension;
  };

  type End = struct {};

  type GIF = struct {
    header              : String(6);
    logicalScreen       : LogicalScreenDescriptor;
    globalColorTable    : GlobalColorTable <| logicalScreen.packedFields & 0x80 != 0;
    blocks              : Block[*];
  };

  let gifParser = parse GIF from "input.gif";
}