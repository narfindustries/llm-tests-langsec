gif {
  header {
    signature: "GIF";
    version: "87a" | "89a";
  }
  logicalScreenDescriptor {
    logicalScreenWidth: U16;
    logicalScreenHeight: U16;
    packedFields {
      globalColorTableFlag: U1;
      colorResolution: U3;
      sortFlag: U1;
      sizeOfGlobalColorTable: U3;
    }
    backgroundColorIndex: U8;
    pixelAspectRatio: U8;
  }
  globalColorTable?: [colorTableEntry] {
    size: 2^(sizeOfGlobalColorTable + 1);
    colorTableEntry {
      red: U8;
      green: U8;
      blue: U8;
    }
  }
  blocks: [block];
  trailer: 0x3B;
}

block {
  imageDescriptor {
    imageSeparator: 0x2C;
    imageLeftPosition: U16;
    imageTopPosition: U16;
    imageWidth: U16;
    imageHeight: U16;
    packedFields {
      localColorTableFlag: U1;
      interlaceFlag: U1;
      sortFlag: U1;
      reserved: U2;
      sizeOfLocalColorTable: U3;
    }
  }
  localColorTable?: [colorTableEntry] {
    size: 2^(sizeOfLocalColorTable + 1);
    colorTableEntry {
      red: U8;
      green: U8;
      blue: U8;
    }
  }
  tableBasedImageData {
    lzwMinimumCodeSize: U8;
    imageDataSubBlocks: [imageDataSubBlock];
  }
  graphicControlExtension?: {
    extensionIntroducer: 0x21;
    graphicControlLabel: 0xF9;
    blockSize: 4;
    packedFields {
      reserved: U3;
      disposalMethod: U3;
      userInputFlag: U1;
      transparencyFlag: U1;
    }
    delayTime: U16;
    transparencyIndex: U8;
    blockTerminator: 0;
  }
  plainTextExtension?: {
    extensionIntroducer: 0x21;
    plainTextLabel: 0x01;
    blockSize: 12;
    textGridLeftPosition: U16;
    textGridTopPosition: U16;
    textGridWidth: U16;
    textGridHeight: U16;
    characterCellWidth: U8;
    characterCellHeight: U8;
    textForegroundColorIndex: U8;
    textBackgroundColorIndex: U8;
    plainTextData: [plainTextDataSubBlock];
    blockTerminator: 0;
  }
  applicationExtension?: {
    extensionIntroducer: 0x21;
    applicationExtensionLabel: 0xFF;
    blockSize: 11;
    applicationIdentifier: [U8; 8];
    applicationAuthenticationCode: [U8; 3];
    applicationData: [applicationDataSubBlock];
    blockTerminator: 0;
  }
  commentExtension?: {
    extensionIntroducer: 0x21;
    commentLabel: 0xFE;
    commentData: [commentDataSubBlock];
    blockTerminator: 0;
  }
}

imageDataSubBlock {
  blockSize: U8;
  data: [U8; blockSize];
}

plainTextDataSubBlock {
  blockSize: U8;
  data: [U8; blockSize];
}

applicationDataSubBlock {
  blockSize: U8;
  data: [U8; blockSize];
}

commentDataSubBlock {
  blockSize: U8;
  data: [U8; blockSize];
}