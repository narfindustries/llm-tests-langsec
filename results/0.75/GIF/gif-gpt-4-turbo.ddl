type U8 = U8;

type U16 = U16 : LittleEndian;

type U32 = U32 : LittleEndian;

type LogicalScreenDescriptor = struct {
  width              U16,
  height             U16,
  packedFields       U8,
  bgColorIndex       U8,
  pixelAspectRatio   U8
};

type ColorTableEntry = struct {
  red   U8,
  green U8,
  blue  U8
};

type GlobalColorTable = array ColorTableEntry;

type ImageDescriptor = struct {
  leftPos   U16,
  topPos    U16,
  width     U16,
  height    U16,
  packed    U8
};

type LocalColorTable = array ColorTableEntry;

type TableBasedImageData = struct {
  lzwMinCodeSize U8,
  imageData      array U8
};

type GraphicControlExtension = struct {
  blockSize       U8,
  packedFields    U8,
  delayTime       U16,
  transparentColorIndex U8,
  terminator      U8
};

type ApplicationExtension = struct {
  blockSize       U8,
  applicationId   array U8,
  appAuthCode     array U8,
  appData         array U8
};

type CommentExtension = struct {
  blockSize   U8,
  commentData array U8
};

type Block = alt {
  ImageDescriptor => struct {
    imageDescriptor ImageDescriptor,
    localColorTable optional LocalColorTable,
    imageData       TableBasedImageData
  },

  Extension => alt {
    GraphicControlExtension => struct {
      introducer   U8 = 0x21,
      label        U8 = 0xF9,
      ext          GraphicControlExtension
    },
    CommentExtension => struct {
      introducer   U8 = 0x21,
      label        U8 = 0xFE,
      comment      CommentExtension
    },
    ApplicationExtension => struct {
      introducer   U8 = 0x21,
      label        U8 = 0xFF,
      appExt       ApplicationExtension
    },
    PlainTextExtension => struct {
      blockSize       U8,
      gridLeftPos     U16,
      gridTopPos      U16,
      gridWidth       U16,
      gridHeight      U16,
      charWidth       U8,
      charHeight      U8,
      textColorIndex  U8,
      bgColorIndex    U8,
      plainTextData   array U8
    }
  }
};

type GIF = struct {
  signature array U8 (len = 3),
  version   array U8 (len = 3),
  logicalScreen LogicalScreenDescriptor,
  globalColorTable optional GlobalColorTable,
  blocks          array Block,
  trailer U8
};