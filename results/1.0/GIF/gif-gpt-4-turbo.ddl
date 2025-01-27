module GIF.GPT4Turbo {
  import common.{Bits8, U16LE, U32LE}

  type ColorTableEntry = struct {
    red   : Bits8
    green : Bits8
    blue  : Bits8
  }

  type LogicalScreenDescriptor = struct {
    canvasWidth        : U16LE
    canvasHeight       : U16LE
    packedField        : Bits8
    bgColorIndex       : Bits8
    pixelAspectRatio   : Bits8
  }

  type GlobalColorTable = struct {
    entries : ColorTableEntry[globalColorTableSize()]
  }

  type ImageDescriptor = struct {
    imageLeftPosition   : U16LE
    imageTopPosition    : U16LE
    imageWidth          : U16LE
    imageHeight         : U16LE
    packedField         : Bits8
  }

  type LocalColorTable = struct {
    entries : ColorTableEntry[localColorTableSize(this._parent.asInstanceOf[ImageDescriptor].packedField)]
  }

  type TableBasedImageData = struct {
    lzwMinimumCodeSize : Bits8
    imageData          : Bytes
  }

  type Block = union {
    case 0x2C => imageDescriptor  : ImageDescriptor
    case 0x21 => extension        : Extension
  }

  type Extension = struct {
    functionCode : Bits8
    blockSize    : Bits8
    data         : Bytes[blockSize.toInt]
    terminator   : Bits8  // Block Terminator (0x00)
  }

  type GIF = struct {
    header              : Stringz("GIF89a" | "GIF87a")
    lSD                 : LogicalScreenDescriptor
    gCT                 : GlobalColorTable[this.lSD.packedField & 0x80 != 0]
    blocks              : Block[0 until EOS]
  }

  private def globalColorTableSize(packed: Bits8): Int = {
    var hasGCT = (packed & 0x80) != 0
    var size = (packed & 0x07) + 1
    if(hasGCT) Math.pow(2, size).toInt else 0
  }

  private def localColorTableSize(packed: Bits8): Int = {
    var hasLCT = (packed & 0x80) != 0
    var size = (packed & 0x07) + 1
    if(hasLCT) Math.pow(2, size).toInt else 0
  }

}