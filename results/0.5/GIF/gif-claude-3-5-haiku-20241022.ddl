type GifHeader {
    signature: [3]u8,
    version: [3]u8
}

type LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packedFields: u8,
    backgroundColorIndex: u8,
    pixelAspectRatio: u8
}

type ColorTableEntry {
    red: u8,
    green: u8,
    blue: u8
}

type ImageDescriptor {
    separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packedFields: u8
}

type ImageData {
    lzwMinCodeSize: u8,
    dataBlocks: [*]DataBlock
}

type DataBlock {
    size: u8,
    data: [size]u8
}

gif: {
    header: GifHeader,
    logicalScreen: LogicalScreenDescriptor,
    globalColorTable: [*]ColorTableEntry if hasGlobalColorTable(logicalScreen.packedFields),
    blocks: [*]Block
}

Block ::= 
    | ImageBlock
    | ExtensionBlock
    | Terminator

ImageBlock: {
    descriptor: ImageDescriptor,
    localColorTable: [*]ColorTableEntry if hasLocalColorTable(descriptor.packedFields),
    imageData: ImageData
}

ExtensionBlock: {
    label: u8,
    data: [*]u8
}

Terminator: {
    marker: u8
}

fn hasGlobalColorTable(packedFields: u8) -> bool {
    return (packedFields & 0x80) != 0
}

fn hasLocalColorTable(packedFields: u8) -> bool {
    return (packedFields & 0x80) != 0
}

parse gif {
    header.signature = "GIF"
    header.version = ["87a" | "89a"]
    
    logicalScreen.width = read u16
    logicalScreen.height = read u16
    logicalScreen.packedFields = read u8
    logicalScreen.backgroundColorIndex = read u8
    logicalScreen.pixelAspectRatio = read u8

    if hasGlobalColorTable(logicalScreen.packedFields) {
        globalColorTable = read [*]ColorTableEntry
    }

    while true {
        match read u8 {
            0x2C => { // Image Separator
                blocks += ImageBlock {
                    descriptor.separator = 0x2C
                    descriptor.left = read u16
                    descriptor.top = read u16
                    descriptor.width = read u16
                    descriptor.height = read u16
                    descriptor.packedFields = read u8

                    if hasLocalColorTable(descriptor.packedFields) {
                        localColorTable = read [*]ColorTableEntry
                    }

                    imageData.lzwMinCodeSize = read u8
                    
                    while true {
                        dataBlock = read DataBlock
                        imageData.dataBlocks += dataBlock
                        if dataBlock.size == 0 { break }
                    }
                }
            }
            0x21 => { // Extension Block
                blocks += ExtensionBlock {
                    label = read u8
                    while true {
                        dataBlock = read DataBlock
                        data += dataBlock.data
                        if dataBlock.size == 0 { break }
                    }
                }
            }
            0x3B => { // Terminator
                blocks += Terminator {
                    marker = 0x3B
                }
                break
            }
        }
    }
}