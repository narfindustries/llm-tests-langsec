type GIF = struct {
    header: Header,
    lsd: LogicalScreenDescriptor,
    gct: GlobalColorTable if header.hasGlobalColorTable,
    blocks: sequence(ImageBlock | ExtensionBlock),
    trailer: Trailer
};

type Header = struct {
    signature: array[3] of u8 where $$ == ['G', 'I', 'F'],
    version: array[3] of u8 where $$ == ['8', '9', 'a'] || $$ == ['8', '7', 'a']
};

type LogicalScreenDescriptor = struct {
    width: u16,
    height: u16,
    packed: PackedField,
    backgroundColorIndex: u8,
    pixelAspectRatio: u8,
    hasGlobalColorTable: packed.globalColorTableFlag == 1
};

type PackedField = struct {
    globalColorTableFlag: bits(1),
    colorResolution: bits(3),
    sortFlag: bits(1),
    globalColorTableSize: bits(3)
};

type GlobalColorTable = struct {
    entries: array[2 ** (parent.lsd.packed.globalColorTableSize + 1)] of RGB
};

type RGB = struct {
    r: u8,
    g: u8,
    b: u8
};

type ImageBlock = struct {
    separator: u8 where $$ == 0x2C,
    leftPosition: u16,
    topPosition: u16,
    width: u16,
    height: u16,
    packed: ImagePackedField,
    lct: LocalColorTable if packed.localColorTableFlag == 1,
    data: ImageData
};

type ImagePackedField = struct {
    localColorTableFlag: bits(1),
    interlaceFlag: bits(1),
    sortFlag: bits(1),
    reserved: bits(2) where $$ == 0,
    localColorTableSize: bits(3)
};

type LocalColorTable = struct {
    entries: array[2 ** (parent.packed.localColorTableSize + 1)] of RGB
};

type ImageData = struct {
    lzwMinimumCodeSize: u8,
    blocks: sequence(DataSubBlock)
};

type ExtensionBlock = struct {
    introducer: u8 where $$ == 0x21,
    label: ExtensionLabel,
    data: ExtensionData
};

type ExtensionLabel = struct {
    value: u8 where $$ in [0xF9, 0xFE, 0x01, 0xFF]
};

type ExtensionData = union {
    0xF9 => GraphicControlExtension,
    0xFE => CommentExtension,
    0x01 => PlainTextExtension,
    0xFF => ApplicationExtension
};

type GraphicControlExtension = struct {
    blockSize: u8 where $$ == 4,
    packed: GCEPackedField,
    delayTime: u16,
    transparentColorIndex: u8,
    terminator: u8 where $$ == 0
};

type GCEPackedField = struct {
    reserved: bits(3) where $$ == 0,
    disposalMethod: bits(3),
    userInputFlag: bits(1),
    transparentColorFlag: bits(1)
};

type CommentExtension = struct {
    blocks: sequence(DataSubBlock),
    terminator: u8 where $$ == 0
};

type PlainTextExtension = struct {
    blockSize: u8 where $$ == 12,
    textGridLeftPosition: u16,
    textGridTopPosition: u16,
    textGridWidth: u16,
    textGridHeight: u16,
    characterCellWidth: u8,
    characterCellHeight: u8,
    textForegroundColorIndex: u8,
    textBackgroundColorIndex: u8,
    blocks: sequence(DataSubBlock),
    terminator: u8 where $$ == 0
};

type ApplicationExtension = struct {
    blockSize: u8 where $$ == 11,
    applicationIdentifier: array[8] of u8,
    applicationAuthCode: array[3] of u8,
    blocks: sequence(DataSubBlock),
    terminator: u8 where $$ == 0
};

type DataSubBlock = struct {
    size: u8,
    data: array[size] of u8 if size > 0
};

type Trailer = struct {
    value: u8 where $$ == 0x3B
};