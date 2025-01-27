def Main = {
  header;
  blocks*;
  trailer
}

def header = {
  $"GIF"; version
}

def version = {
  $"87a" | $"89a"
}

def blocks = {
  logicalScreenDescriptor;
  globalColorTable?;
  (graphicBlock | specialPurposeBlock)*
}

def logicalScreenDescriptor = {
  width: uint16LE;
  height: uint16LE;
  flags: uint8;
  bgColorIndex: uint8;
  pixelAspectRatio: uint8
}

def globalColorTable = {
  @colorTableSize = (1 << ((flags & 0x07) + 1));
  colorTableEntry[colorTableSize]
}

def colorTableEntry = {
  r: uint8;
  g: uint8;
  b: uint8
}

def graphicBlock = {
  imageBlock | extensionBlock
}

def imageBlock = {
  $0x2C;
  left: uint16LE;
  top: uint16LE;
  width: uint16LE;
  height: uint16LE;
  flags: uint8;
  localColorTable?;
  imageData
}

def localColorTable = {
  @localTableSize = (1 << ((flags & 0x07) + 1));
  colorTableEntry[localTableSize]
}

def imageData = {
  lzwMinCodeSize: uint8;
  subBlocks
}

def extensionBlock = {
  $0x21;
  (graphicControlExtension |
   commentExtension |
   plainTextExtension |
   applicationExtension)
}

def graphicControlExtension = {
  $0xF9;
  blockSize: uint8;
  flags: uint8;
  delayTime: uint16LE;
  transparentColorIndex: uint8;
  terminator: uint8
}

def commentExtension = {
  $0xFE;
  subBlocks
}

def plainTextExtension = {
  $0x01;
  blockSize: uint8;
  textGridLeft: uint16LE;
  textGridTop: uint16LE;
  textGridWidth: uint16LE;
  textGridHeight: uint16LE;
  cellWidth: uint8;
  cellHeight: uint8;
  textFgColorIndex: uint8;
  textBgColorIndex: uint8;
  subBlocks
}

def applicationExtension = {
  $0xFF;
  blockSize: uint8;
  applicationId: uint8[8];
  applicationAuthCode: uint8[3];
  subBlocks
}

def subBlocks = {
  (subBlock)*;
  $0x00
}

def subBlock = {
  size: uint8;
  data: uint8[size]
}

def specialPurposeBlock = {
  $0x21;
  label: uint8;
  subBlocks
}

def trailer = {
  $0x3B
}