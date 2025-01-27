module GIF;

type Gif = struct {
    header: Header,
    logicalScreenDescriptor: LogicalScreenDescriptor,
    globalColorTable: GlobalColorTable?,
    blocks: list<Block>
};

type Header = struct {
    signature: string[3] "GIF",
    version: string[3] { oneOf: ["87a", "89a"] }
};

type LogicalScreenDescriptor = struct {
    width: uint16,
    height: uint16,
    packedFields: PackedFields,
    backgroundColorIndex: uint8,
    pixelAspectRatio: uint8
};

type PackedFields = struct {
    globalColorTableFlag: bool,
    colorResolution: uint3,
    sortFlag: bool,
    sizeOfGlobalColorTable: uint3
};

type GlobalColorTable = list<Color>;

type Color = struct {
    red: uint8,
    green: uint8,
    blue: uint8
};

type Block = oneOf {
    extensionBlock: ExtensionBlock,
    imageBlock: ImageBlock,
    trailer: Trailer
};

type ExtensionBlock = struct {
    introducer: uint8 0x21,
    label: uint8,
    data: ExtensionData
};

type ExtensionData = oneOf {
    graphicControlExtension: GraphicControlExtension,
    commentExtension: CommentExtension,
    applicationExtension: ApplicationExtension,
    plainTextExtension: PlainTextExtension
};

type GraphicControlExtension = struct {
    blockSize: uint8 0x04,
    packedFields: GraphicControlPackedFields,
    delayTime: uint16,
    transparentColorIndex: uint8,
    blockTerminator: uint8 0x00
};

type GraphicControlPackedFields = struct {
    reserved: uint3,
    disposalMethod: uint3,
    userInputFlag: bool,
    transparentColorFlag: bool
};

type CommentExtension = struct {
    commentData: SubBlocks
};

type ApplicationExtension = struct {
    blockSize: uint8 0x0B,
    applicationIdentifier: string[8],
    applicationAuthCode: string[3],
    applicationData: SubBlocks
};

type PlainTextExtension = struct {
    blockSize: uint8 0x0C,
    textGridLeft: uint16,
    textGridTop: uint16,
    textGridWidth: uint16,
    textGridHeight: uint16,
    characterCellWidth: uint8,
    characterCellHeight: uint8,
    textForegroundColorIndex: uint8,
    textBackgroundColorIndex: uint8,
    plainTextData: SubBlocks
};

type ImageBlock = struct {
    separator: uint8 0x2C,
    imageDescriptor: ImageDescriptor,
    localColorTable: LocalColorTable?,
    imageData: ImageData
};

type ImageDescriptor = struct {
    leftPosition: uint16,
    topPosition: uint16,
    width: uint16,
    height: uint16,
    packedFields: ImagePackedFields
};

type ImagePackedFields = struct {
    localColorTableFlag: bool,
    interlaceFlag: bool,
    sortFlag: bool,
    reserved: uint2,
    sizeOfLocalColorTable: uint3
};

type LocalColorTable = list<Color>;

type ImageData = struct {
    lzwMinimumCodeSize: uint8,
    imageDataBlocks: SubBlocks
};

type SubBlocks = list<SubBlock>;

type SubBlock = struct {
    size: uint8,
    data: bytes[size]
};

type Trailer = struct {
    terminator: uint8 0x3B
};