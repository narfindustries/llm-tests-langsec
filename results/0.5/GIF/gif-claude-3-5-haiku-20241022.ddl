grammar GIF {
    rule File = {
        signature: bytes[3] where
            signature[0] == 0x47 &&
            signature[1] == 0x49 &&
            signature[2] == 0x46

        version: bytes[3] where
            (version[0] == 0x38 && version[1] == 0x37 && version[2] == 0x61) ||
            (version[0] == 0x38 && version[1] == 0x39 && version[2] == 0x61)

        screenDescriptor: {
            width: u16
            height: u16
            packedFields: {
                globalColorTableFlag: u1
                colorResolution: u3
                sortFlag: u1
                globalColorTableSize: u3
            }
            backgroundColorIndex: u8
            pixelAspectRatio: u8
        }

        globalColorTable: optional<ColorTable> where
            screenDescriptor.packedFields.globalColorTableFlag == 1

        blocks: list<Block>
        trailer: Trailer
    }

    rule ColorTable = {
        entries: list<RGBColor>
    }

    rule RGBColor = {
        red: u8
        green: u8
        blue: u8
    }

    rule Block = 
        | ImageBlock
        | ExtensionBlock

    rule ImageBlock = {
        separator: u8 where separator == 0x2C
        leftPosition: u16
        topPosition: u16
        width: u16
        height: u16
        packedFields: {
            localColorTableFlag: u1
            interlaceFlag: u1
            sortFlag: u1
            localColorTableSize: u3
        }
        localColorTable: optional<ColorTable> where
            packedFields.localColorTableFlag == 1
        lzwMinCodeSize: u8
        imageData: CompressedData
    }

    rule CompressedData = {
        blocks: list<DataBlock>
        terminatorBlock: u8 where terminatorBlock == 0x00
    }

    rule DataBlock = {
        length: u8
        data: bytes[length] where length > 0
    }

    rule ExtensionBlock = {
        introducer: u8 where introducer == 0x21
        extensionType: u8
        extensionData: ExtensionData
    }

    rule ExtensionData = 
        | GraphicControlExtension
        | CommentExtension
        | PlainTextExtension
        | ApplicationExtension

    rule GraphicControlExtension = {
        blockSize: u8
        packedFields: u8
        delayTime: u16
        transparentColorIndex: u8
        terminatorBlock: u8 where terminatorBlock == 0x00
    }

    rule CommentExtension = {
        blocks: list<DataBlock>
        terminatorBlock: u8 where terminatorBlock == 0x00
    }

    rule PlainTextExtension = {
        blockSize: u8
        textGridLeft: u16
        textGridTop: u16
        textGridWidth: u16
        textGridHeight: u16
        cellWidth: u8
        cellHeight: u8
        foregroundColorIndex: u8
        backgroundColorIndex: u8
        textData: list<DataBlock>
        terminatorBlock: u8 where terminatorBlock == 0x00
    }

    rule ApplicationExtension = {
        blockSize: u8
        identifier: bytes[8]
        authenticationCode: bytes[3]
        applicationData: list<DataBlock>
        terminatorBlock: u8 where terminatorBlock == 0x00
    }

    rule Trailer = {
        marker: u8 where marker == 0x3B
    }
}