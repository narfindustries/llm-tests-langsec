PNG = struct {
    signature: u8[8] == [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
    chunks: Chunk[];
}

Chunk = struct {
    length: u32;
    type: ChunkType;
    data: u8[length];
    crc: u32;
}

ChunkType = enum : u32 {
    IHDR = 0x49484452,
    PLTE = 0x504C5445,
    IDAT = 0x49444154,
    IEND = 0x49454E44,
    tRNS = 0x74524E53,
    cHRM = 0x6348524D,
    gAMA = 0x67414D41,
    iCCP = 0x69434350,
    sBIT = 0x73424954,
    sRGB = 0x73524742,
    bKGD = 0x624B4744,
    hIST = 0x68495354,
    pHYs = 0x70485973,
    sPLT = 0x73504C54,
    tIME = 0x74494D45,
    tEXt = 0x74455874,
    zTXt = 0x7A545874,
    iTXt = 0x69545874
}

IHDR = struct {
    width: u32;
    height: u32;
    bitDepth: u8;
    colorType: ColorType;
    compressionMethod: u8 == 0;
    filterMethod: u8 == 0;
    interlaceMethod: InterlaceMethod;
}

ColorType = enum : u8 {
    Grayscale = 0,
    Truecolor = 2,
    IndexedColor = 3,
    GrayscaleAlpha = 4,
    TruecolorAlpha = 6
}

InterlaceMethod = enum : u8 {
    None = 0,
    Adam7 = 1
}

PLTE = struct {
    entries: RGB[(length / 3)];
}

RGB = struct {
    red: u8;
    green: u8;
    blue: u8;
}

IDAT = struct {
    compressedData: u8[length];
}

IEND = struct {
    // No data
}

tRNS = struct {
    transparencyData: u8[length];
}

cHRM = struct {
    whitePointX: u32;
    whitePointY: u32;
    redX: u32;
    redY: u32;
    greenX: u32;
    greenY: u32;
    blueX: u32;
    blueY: u32;
}

gAMA = struct {
    gamma: u32;
}

iCCP = struct {
    profileName: u8[];
    compressionMethod: u8;
    compressedProfile: u8[];
}

sBIT = struct {
    significantBits: u8[length];
}

sRGB = struct {
    renderingIntent: u8;
}

bKGD = struct {
    backgroundColor: u8[length];
}

hIST = struct {
    frequencies: u16[(length / 2)];
}

pHYs = struct {
    pixelsPerUnitX: u32;
    pixelsPerUnitY: u32;
    unitSpecifier: u8;
}

sPLT = struct {
    paletteName: u8[];
    sampleDepth: u8;
    paletteEntries: PaletteEntry[];
}

PaletteEntry = struct {
    red: u8;
    green: u8;
    blue: u8;
    alpha: u8;
    frequency: u16;
}

tIME = struct {
    year: u16;
    month: u8;
    day: u8;
    hour: u8;
    minute: u8;
    second: u8;
}

tEXt = struct {
    keyword: u8[];
    text: u8[];
}

zTXt = struct {
    keyword: u8[];
    compressionMethod: u8;
    compressedText: u8[];
}

iTXt = struct {
    compressed: u8;
    compressionMethod: u8;
    languageTag: u8[];
    translatedKeyword: u8[];
    text: u8[];
}