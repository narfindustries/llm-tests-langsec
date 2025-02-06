def Main = {
    Header
    LogicalScreenDescriptor
    GlobalColorTableOpt
    ExtensionsAndBlocks
    Trailer
}

def Header = {
    signature: BYTE[3]
    version: BYTE[3]
}

def LogicalScreenDescriptor = {
    width: UINT16
    height: UINT16
    packed: UINT8
    bgColorIndex: UINT8
    pixelAspectRatio: UINT8
}

def gctFlag = (packed & 0x80) >> 7
def colorRes = (packed & 0x70) >> 4
def sortFlag = (packed & 0x08) >> 3
def gctSize = packed & 0x07

def GlobalColorTableOpt = {
    if (gctFlag == 1) {
        colors: RGB[2 ** (gctSize + 1)]
    }
}

def RGB = {
    r: UINT8
    g: UINT8
    b: UINT8
}

def ExtensionsAndBlocks = {
    while (!Lookahead(Trailer)) {
        GraphicBlock | Extension
    }
}

def GraphicBlock = {
    ImageDescriptor
    LocalColorTableOpt
    ImageData
}

def ImageDescriptor = {
    separator: 0x2C
    left: UINT16
    top: UINT16
    width: UINT16
    height: UINT16
    imagePacked: UINT8
}

def lctFlag = (imagePacked & 0x80) >> 7
def interlaceFlag = (imagePacked & 0x40) >> 6
def sortFlagLocal = (imagePacked & 0x20) >> 5
def lctSize = imagePacked & 0x07

def LocalColorTableOpt = {
    if (lctFlag == 1) {
        colors: RGB[2 ** (lctSize + 1)]
    }
}

def ImageData = {
    lzwMinCodeSize: UINT8
    DataSubBlocks
}

def Extension = {
    introducer: 0x21
    label: UINT8
    match label {
        0xF9 => GraphicsControlExtension
        0xFE => CommentExtension
        0x01 => PlainTextExtension
        0xFF => ApplicationExtension
    }
}

def GraphicsControlExtension = {
    blockSize: 4
    gcePacked: UINT8
    delayTime: UINT16
    transparentColorIndex: UINT8
    terminator: 0x00
}

def CommentExtension = {
    DataSubBlocks
    terminator: 0x00
}

def PlainTextExtension = {
    blockSize: 12
    textGridLeft: UINT16
    textGridTop: UINT16
    textGridWidth: UINT16
    textGridHeight: UINT16
    cellWidth: UINT8
    cellHeight: UINT8
    fgColorIndex: UINT8
    bgColorIndex: UINT8
    DataSubBlocks
    terminator: 0x00
}

def ApplicationExtension = {
    blockSize: 11
    identifier: BYTE[8]
    authCode: BYTE[3]
    DataSubBlocks
    terminator: 0x00
}

def DataSubBlocks = {
    while (!Lookahead(0x00)) {
        DataSubBlock
    }
}

def DataSubBlock = {
    size: UINT8
    Guard size > 0
    data: BYTE[size]
}

def Trailer = {
    trailer: 0x3B
}