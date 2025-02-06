module gif

struct GIFFile
    Header header
    LogicalScreenDescriptor screenDescriptor
    if screenDescriptor.globalColorTableFlag
        ColorTable globalColorTable(screenDescriptor.sizeOfGlobalColorTable)
    while !endOfFile()
        Block block

struct Header
    char[3] signature // "GIF"
    char[3] version   // "87a" or "89a"

struct LogicalScreenDescriptor
    uint16 screenWidth
    uint16 screenHeight
    uint8  packedFields
    uint8  backgroundColorIndex
    uint8  pixelAspectRatio

    bool globalColorTableFlag = (packedFields & 0x80) != 0
    uint8 colorResolution = ((packedFields & 0x70) >> 4) + 1
    bool sortFlag = (packedFields & 0x08) != 0
    uint8 sizeOfGlobalColorTable = (packedFields & 0x07)

struct ColorTable
    ColorTable(uint8 size)
        entries = [ColorTableEntry; 2 << size]
    ColorTableEntry entries[]

struct ColorTableEntry
    uint8 red
    uint8 green
    uint8 blue

struct Block
    uint8 blockIdentifier

    switch blockIdentifier
        case 0x2C: ImageBlock imageBlock
        case 0x21: ExtensionBlock extensionBlock
        case 0x3B: TrailerBlock trailerBlock

struct ImageBlock
    ImageDescriptor imageDescriptor
    if imageDescriptor.localColorTableFlag
        ColorTable localColorTable(imageDescriptor.sizeOfLocalColorTable)
    ImageData imageData

struct ImageDescriptor
    uint16 imageLeftPosition
    uint16 imageTopPosition
    uint16 imageWidth
    uint16 imageHeight
    uint8  packedFields

    bool localColorTableFlag = (packedFields & 0x80) != 0
    bool interlaceFlag = (packedFields & 0x40) != 0
    bool sortFlag = (packedFields & 0x20) != 0
    uint8 sizeOfLocalColorTable = (packedFields & 0x07)

struct ImageData
    uint8 lzwMinimumCodeSize
    DataSubBlocks dataSubBlocks

struct DataSubBlocks
    while true
        uint8 blockSize
        if blockSize == 0 break
        uint8[blockSize] blockData

struct ExtensionBlock
    uint8 extensionLabel

    switch extensionLabel
        case 0xF9: GraphicControlExtension graphicControlExtension
        case 0xFE: CommentExtension commentExtension
        case 0xFF: ApplicationExtension applicationExtension
        case 0x01: PlainTextExtension plainTextExtension

struct GraphicControlExtension
    uint8 blockSize // Always 4
    uint8 packedFields
    uint16 delayTime
    uint8 transparentColorIndex
    uint8 blockTerminator // Always 0x00

    uint8 disposalMethod = (packedFields & 0x1C) >> 2
    bool userInputFlag = (packedFields & 0x02) != 0
    bool transparentColorFlag = (packedFields & 0x01) != 0

struct CommentExtension
    DataSubBlocks commentData

struct ApplicationExtension
    uint8 blockSize // Always 11
    char[8] applicationIdentifier
    char[3] applicationAuthCode
    DataSubBlocks applicationData

struct PlainTextExtension
    uint8 blockSize // Always 12
    uint16 textGridLeftPosition
    uint16 textGridTopPosition
    uint16 textGridWidth
    uint16 textGridHeight
    uint8 characterCellWidth
    uint8 characterCellHeight
    uint8 textForegroundColorIndex
    uint8 textBackgroundColorIndex
    DataSubBlocks plainTextData

struct TrailerBlock
    // No additional data, just indicates end of GIF file