type PngSignature = { magic: [u8; 8] }

fn validate_signature(sig: PngSignature) -> bool {
    sig.magic[0] == 0x89 && 
    sig.magic[1] == 0x50 && 
    sig.magic[2] == 0x4E && 
    sig.magic[3] == 0x47 && 
    sig.magic[4] == 0x0D && 
    sig.magic[5] == 0x0A && 
    sig.magic[6] == 0x1A && 
    sig.magic[7] == 0x0A
}

enum ColorType {
    Grayscale = 0,
    RGB = 2,
    Palette = 3,
    GrayscaleAlpha = 4,
    RGBAlpha = 6
}

enum CompressionMethod {
    Deflate = 0
}

enum FilterMethod {
    Adaptive = 0
}

enum InterlaceMethod {
    None = 0,
    Adam7 = 1
}

type Chunk = {
    length: u32,
    type: [u8; 4],
    data: [u8; length],
    crc: u32
}

type ImageHeader = {
    width: u32,
    height: u32,
    bitDepth: u8,
    colorType: ColorType,
    compressionMethod: CompressionMethod,
    filterMethod: FilterMethod,
    interlaceMethod: InterlaceMethod
} where 
    width > 0 && width <= 0x7FFFFFFF &&
    height > 0 && height <= 0x7FFFFFFF &&
    bitDepth in [1, 2, 4, 8, 16]

type Palette = {
    entries: [[u8; 3]]
}

type Transparency = {
    grayAlpha: [u8; 2] if colorType == ColorType.Grayscale,
    rgbAlpha: [u8; 6] if colorType == ColorType.RGB,
    paletteAlpha: [u8] if colorType == ColorType.Palette
}

type Chromaticity = {
    whitePointX: u32,
    whitePointY: u32,
    redX: u32,
    redY: u32,
    greenX: u32,
    greenY: u32,
    blueX: u32,
    blueY: u32
}

type Gamma = {
    gammaValue: u32
}

type PhysicalPixelDimensions = {
    pixelsPerUnitX: u32,
    pixelsPerUnitY: u32,
    unitSpecifier: u8
}

type BackgroundColor = {
    backgroundPaletteIndex: u8 if colorType == ColorType.Palette,
    backgroundGrayscale: u16 if colorType == ColorType.Grayscale,
    backgroundRGB: [u16; 3] if colorType == ColorType.RGB
}

type TimeChunk = {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8
}

type TextChunk = {
    keyword: string,
    text: string
}

type CompressedTextChunk = {
    keyword: string,
    compressionMethod: u8,
    compressedText: [u8]
}

type InternationalTextChunk = {
    keyword: string,
    compressionFlag: u8,
    compressionMethod: u8,
    languageTag: string,
    translatedKeyword: string,
    text: string
}

type PngImage = {
    signature: PngSignature where validate_signature(signature),
    chunks: [Chunk],
    imageHeader: ImageHeader,
    palette: Palette?,
    transparency: Transparency?,
    chromaticity: Chromaticity?,
    gamma: Gamma?,
    physicalPixelDimensions: PhysicalPixelDimensions?,
    backgroundColor: BackgroundColor?,
    timeChunk: TimeChunk?,
    textChunks: [TextChunk],
    compressedTextChunks: [CompressedTextChunk],
    internationalTextChunks: [InternationalTextChunk],
    imageData: [u8],
    imageTrailer: Chunk
}