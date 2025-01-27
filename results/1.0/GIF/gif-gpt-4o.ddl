Root {
    width: uint16;
    height: uint16;
    globalColorTableFlag: bool;
    colorResolution: uint3;
    sortFlag: bool;
    sizeOfGlobalColorTable: uint3;
    backgroundColorIndex: uint8;
    pixelAspectRatio: uint8;
    globalColorTable: if(globalColorTableFlag) [Size]Color;
    blocks: *Block;
}

Color {
    red: uint8;
    green: uint8;
    blue: uint8;
}

Block {
    block_type: uint8 {
        0x2C -> ImageBlock,
        0x21 -> ExtensionBlock,
        0x3B -> TrailerBlock,
    }
}

ImageBlock {
    imageLeft: uint16;
    imageTop: uint16;
    imageWidth: uint16;
    imageHeight: uint16;
    localColorTableFlag: bool;
    interlaceFlag: bool;
    sortFlag: bool;
    reserved: uint2;
    sizeOfLocalColorTable: uint3;
    localColorTable: if(localColorTableFlag) [Size]Color;
    lzwMinCodeSize: uint8;
    imageData: CompressedData;
}

CompressedData {
    subBlocks: *SubBlock;
}

SubBlock {
    size: uint8;
    data: [size]byte;
}

ExtensionBlock {
    extensionLabel: uint8 {
        0xF9 -> GraphicControlExtension,
        0xFE -> CommentExtension,
        0x01 -> PlainTextExtension,
        0xFF -> ApplicationExtension,
    }
}

GraphicControlExtension {
    blockSize: uint8 = 4;
    reserved: uint3;
    disposalMethod: uint3;
    userInputFlag: bool;
    transparentColorFlag: bool;
    delayTime: uint16;
    transparentColorIndex: uint8;
    blockTerminator: uint8 = 0;
}

CommentExtension {
    commentData: CompressedData;
}

PlainTextExtension {
    blockSize: uint8 = 12;
    textGridLeft: uint16;
    textGridTop: uint16;
    textGridWidth: uint16;
    textGridHeight: uint16;
    characterCellWidth: uint8;
    characterCellHeight: uint8;
    textForegroundColorIndex: uint8;
    textBackgroundColorIndex: uint8;
    plainTextData: CompressedData;
}

ApplicationExtension {
    blockSize: uint8 = 11;
    applicationIdentifier: [8]char;
    applicationAuthCode: [3]char;
    applicationData: CompressedData;
}

TrailerBlock {
    trailer: uint8 = 0x3B;
}