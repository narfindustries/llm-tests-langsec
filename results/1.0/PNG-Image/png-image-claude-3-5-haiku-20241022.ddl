def PngSignature = {
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A
}

type ColorType =
    | Grayscale
    | RGB
    | Palette
    | GrayscaleAlpha
    | RGBAlpha

type InterlaceMethod =
    | NoInterlace
    | Adam7Interlace

type CompressionMethod =
    | Deflate

type FilterMethod =
    | AdaptiveFiltering

type RenderingIntent =
    | Perceptual
    | RelativeColorimetric
    | Saturation
    | AbsoluteColorimetric

type ChunkType =
    | IHDR
    | PLTE
    | IDAT
    | IEND
    | tRNS
    | cHRM
    | gAMA
    | iCCP
    | sBIT
    | sRGB
    | tEXt
    | zTXt
    | iTXt
    | bKGD
    | pHYs
    | sTER

type PngChunk = {
    length: U32,
    type: ChunkType,
    data: Bytes(length),
    crc: U32
}

type ImageHeader = {
    width: U32 where width > 0 and width < 2^31,
    height: U32 where height > 0 and height < 2^31,
    bitDepth: U8 where bitDepth in [1, 2, 4, 8, 16],
    colorType: ColorType,
    compressionMethod: CompressionMethod,
    filterMethod: FilterMethod,
    interlaceMethod: InterlaceMethod
}

type Palette = {
    entries: List<(U8, U8, U8)> where len(entries) <= 256
}

type Transparency = 
    | PaletteAlpha of List<U8>
    | GrayscaleAlpha of U16
    | RGBAlpha of (U16, U16, U16)

type ChromaticityPoints = {
    whiteX: U32,
    whiteY: U32,
    redX: U32,
    redY: U32,
    greenX: U32,
    greenY: U32,
    blueX: U32,
    blueY: U32
}

type PhysicalDimensions = {
    pixelsPerUnitX: U32,
    pixelsPerUnitY: U32,
    unitSpecifier: U8
}

type StandardRGB = {
    renderingIntent: RenderingIntent
}

type TextChunk = {
    keyword: Str,
    text: Str
}

type CompressedTextChunk = {
    keyword: Str,
    compressionMethod: U8,
    compressedText: Bytes
}

type InternationalTextChunk = {
    keyword: Str,
    compressionFlag: U8,
    compressionMethod: U8,
    languageTag: Str,
    translatedKeyword: Str,
    text: Str
}

type BackgroundColor = 
    | PaletteBackground of U8
    | GrayscaleBackground of U16
    | RGBBackground of (U16, U16, U16)

type PngFile = {
    signature: PngSignature,
    chunks: List<PngChunk> where
        chunks[0].type == ChunkType.IHDR and
        chunks[len(chunks)-1].type == ChunkType.IEND
}