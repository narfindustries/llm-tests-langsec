enum u3 DisposalMethod {
    NoAction = 0,
    DoNotDispose = 1,
    RestoreToBackground = 2,
    RestoreToPrevious = 3,
    Reserved = 4..7
}

struct GIF {
    Header {
        signature : u8[3] == "GIF";
        version : u8[3] == "89a";
    }

    LogicalScreenDescriptor {
        logicalScreenWidth : u16;
        logicalScreenHeight : u16;
        packedFields : u8 {
            globalColorTableFlag : bool @ 7;
            colorResolution : u3 @ 4;
            sortFlag : bool @ 3;
            sizeOfGlobalColorTable : u3;
        }
        backgroundColorIndex : u8;
        pixelAspectRatio : u8;
    }

    GlobalColorTable(size : u3) [if .LogicalScreenDescriptor.packedFields.globalColorTableFlag] {
        colors : u8[3][2 ** (size + 1)];
    }

    Block {
        while (true) {
            blockType : u8;
            switch (blockType) {
                case 0x2C: ImageDescriptor;
                case 0x21: Extension;
                case 0x3B: break;
                default: error("Unknown block type");
            }
        }
    }

    ImageDescriptor {
        imageSeparator : u8 == 0x2C;
        imageLeftPosition : u16;
        imageTopPosition : u16;
        imageWidth : u16;
        imageHeight : u16;
        packedFields : u8 {
            localColorTableFlag : bool @ 7;
            interlaceFlag : bool @ 6;
            sortFlag : bool @ 5;
            reserved : u2 @ 3;
            sizeOfLocalColorTable : u3;
        }
        LocalColorTable(size : u3) [if .packedFields.localColorTableFlag] {
            colors : u8[3][2 ** (size + 1)];
        }
        ImageData;
    }

    ImageData {
        lzwMinimumCodeSize : u8;
        while (true) {
            blockSize : u8;
            if (blockSize == 0) break;
            imageData : u8[blockSize];
        }
    }

    Extension {
        extensionIntroducer : u8 == 0x21;
        extensionLabel : u8;
        switch (extensionLabel) {
            case 0xF9: GraphicControlExtension;
            case 0xFE: CommentExtension;
            case 0x01: PlainTextExtension;
            case 0xFF: ApplicationExtension;
            default: error("Unknown extension label");
        }
    }

    GraphicControlExtension {
        blockSize : u8 == 4;
        packedFields : u8 {
            reserved : u3 @ 5;
            disposalMethod : DisposalMethod @ 2;
            userInputFlag : bool @ 1;
            transparentColorFlag : bool;
        }
        delayTime : u16;
        transparentColorIndex : u8;
        blockTerminator : u8 == 0;
    }

    CommentExtension {
        while (true) {
            blockSize : u8;
            if (blockSize == 0) break;
            commentData : u8[blockSize];
        }
    }

    PlainTextExtension {
        blockSize : u8 == 12;
        textGridLeftPosition : u16;
        textGridTopPosition : u16;
        textGridWidth : u16;
        textGridHeight : u16;
        characterCellWidth : u8;
        characterCellHeight : u8;
        textForegroundColorIndex : u8;
        textBackgroundColorIndex : u8;
        while (true) {
            blockSize : u8;
            if (blockSize == 0) break;
            plainTextData : u8[blockSize];
        }
    }

    ApplicationExtension {
        blockSize : u8 == 11;
        applicationIdentifier : u8[8];
        applicationAuthenticationCode : u8[3];
        while (true) {
            blockSize : u8;
            if (blockSize == 0) break;
            applicationData : u8[blockSize];
        }
    }

    Trailer {
        trailer : u8 == 0x3B;
    }
}