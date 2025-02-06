gif = {
  header = {
    signature: U8[3] = "GIF";
    version: U8[3] = "89a";
  };
  logicalScreenDescriptor = {
    logicalScreenWidth: U16;
    logicalScreenHeight: U16;
    packedFields: U8 {
      globalColorTableFlag: Bit;
      colorResolution: Bits[3];
      sortFlag: Bit;
      sizeOfGlobalColorTable: Bits[3];
    };
    backgroundColorIndex: U8;
    pixelAspectRatio: U8;
  };
  globalColorTable: If(logicalScreenDescriptor.packedFields.globalColorTableFlag) = {
    colors: U8[3 * (1 << (logicalScreenDescriptor.packedFields.sizeOfGlobalColorTable + 1))];
  };
  blocks: RepeatUntil(_.blockType == 0x3B) = {
    blockType: U8;
    block: Switch(blockType) = {
      case 0x2C: imageDescriptor = {
        imageLeftPosition: U16;
        imageTopPosition: U16;
        imageWidth: U16;
        imageHeight: U16;
        packedFields: U8 {
          localColorTableFlag: Bit;
          interlaceFlag: Bit;
          sortFlag: Bit;
          reserved: Bits[2];
          sizeOfLocalColorTable: Bits[3];
        };
        localColorTable: If(packedFields.localColorTableFlag) = {
          colors: U8[3 * (1 << (packedFields.sizeOfLocalColorTable + 1))];
        };
        lzwMinimumCodeSize: U8;
        imageData: RepeatUntil(_.subBlockSize == 0) = {
          subBlockSize: U8;
          subBlockData: U8[subBlockSize];
        };
      };
      case 0x21: extension = {
        extensionLabel: U8;
        extensionData: Switch(extensionLabel) = {
          case 0xF9: graphicControlExtension = {
            blockSize: U8 = 4;
            packedFields: U8 {
              reserved: Bits[3];
              disposalMethod: Bits[3];
              userInputFlag: Bit;
              transparencyFlag: Bit;
            };
            delayTime: U16;
            transparentColorIndex: U8;
            blockTerminator: U8 = 0;
          };
          case 0x01: plainTextExtension = {
            blockSize: U8 = 12;
            textGridLeftPosition: U16;
            textGridTopPosition: U16;
            textGridWidth: U16;
            textGridHeight: U16;
            characterCellWidth: U8;
            characterCellHeight: U8;
            textForegroundColorIndex: U8;
            textBackgroundColorIndex: U8;
            plainTextData: RepeatUntil(_.subBlockSize == 0) = {
              subBlockSize: U8;
              subBlockData: U8[subBlockSize];
            };
            blockTerminator: U8 = 0;
          };
          case 0xFF: applicationExtension = {
            blockSize: U8 = 11;
            applicationIdentifier: U8[8];
            applicationAuthenticationCode: U8[3];
            applicationData: RepeatUntil(_.subBlockSize == 0) = {
              subBlockSize: U8;
              subBlockData: U8[subBlockSize];
            };
            blockTerminator: U8 = 0;
          };
          case 0xFE: commentExtension = {
            commentData: RepeatUntil(_.subBlockSize == 0) = {
              subBlockSize: U8;
              subBlockData: U8[subBlockSize];
            };
            blockTerminator: U8 = 0;
          };
        };
      };
    };
  };
  trailer: U8 = 0x3B;
};