module GIF.GPT4Turbo {

  import DAEDALUS::Core;

  struct RGB {
    r : U8;
    g : U8;
    b : U8;
  }

  struct Header {
    signature : String[3];
    version : String[3];
  }

  struct LogicalScreenDescriptor {
    logicalScreenWidth : U16;
    logicalScreenHeight : U16;
    packedFields : U8;
    backgroundColorIndex : U8;
    pixelAspectRatio : U8;
  }

  struct ColorTableEntry {
    red : U8;
    green : U8;
    blue : U8;
  }

  type ColorTable = [ColorTableEntry];

  struct ImageDescriptor {
    imageSeparator : U8;
    imageLeftPosition : U16;
    imageTopPosition : U16;
    imageWidth : U16;
    imageHeight : U16;
    packedFields : U8;
  }

  struct GraphicControlExtension {
    introducer : U8;
    label : U8;
    blockSize : U8;
    packedFields : U8;
    delayTime : U16;
    transparentColorIndex : U8;
    terminator : U8;
  }

  struct ApplicationExtension {
    introducer : U8;
    label : U8;
    blockSize : U8;
    applicationIdentifier : String[8];
    applicationAuthCode : String[3];
    data : [U8];
  }

  struct CommentExtension {
    introducer : U8;
    label : U8;
    blockSize : U8;
    commentData : String;
  }

  struct Block {
    blockSize : U8;
    blockData : [U8] @Length(blockSize);
  }

  type SubBlocks = [Block];

  struct Trailer {
    trailer : U8;
  }

  struct GIFFile {
    header : Header;
    logicalScreenDescriptor : LogicalScreenDescriptor;
    globalColorTable : ColorTable @If(logicalScreenDescriptor.packedFields & 0x80 != 0) @Length(2 ^ ((logicalScreenDescriptor.packedFields & 0x07) + 1));
    blocks : [Variant [
      GraphicControlExtension @Match(block -> block.introducer == 0x21 && block.label == 0xF9),
      CommentExtension @Match(block -> block.introducer == 0x21 && block.label == 0xFE),
      ApplicationExtension @Match(block -> block.introducer == 0x21 && block.label == 0xFF),
      ImageDescriptor @Match(block -> block.imageSeparator == 0x2C),
      SubBlocks @Match(block -> block.blockSize != 0)
    ]];
    trailer : Trailer;
  }
}