PNG : struct {
    signature : u8[8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A],
    chunks : list(Chunk),
}

ChunkType : enum(u32) {
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
    tEXt = 0x74455874,
    zTXt = 0x7A545874,
    iTXt = 0x69545874,
    bKGD = 0x624B4744,
    pHYs = 0x70485973,
    sPLT = 0x73504C54,
    hIST = 0x68495354,
    tIME = 0x74494D45,
}

Chunk : struct {
    length : u32,
    type : ChunkType,
    data : u8[length],
    crc : u32,
}

IHDR : struct {
    width : u32,
    height : u32,
    bit_depth : u8,
    color_type : ColorType,
    compression_method : u8 = 0,
    filter_method : u8 = 0,
    interlace_method : InterlaceMethod,
}

ColorType : enum(u8) {
    Grayscale = 0,
    Truecolor = 2,
    IndexedColor = 3,
    GrayscaleAlpha = 4,
    TruecolorAlpha = 6,
}

InterlaceMethod : enum(u8) {
    None = 0,
    Adam7 = 1,
}

PLTE : struct {
    entries : list(RGB),
}

RGB : struct {
    r : u8,
    g : u8,
    b : u8,
}

IDAT : struct {
    compressed_data : u8[],
}

tRNS : struct {
    grayscale : u16 if IHDR.color_type == ColorType.Grayscale,
    rgb : RGB if IHDR.color_type == ColorType.Truecolor,
    alpha_values : u8[] if IHDR.color_type == ColorType.IndexedColor,
}

cHRM : struct {
    white_point_x : u32,
    white_point_y : u32,
    red_x : u32,
    red_y : u32,
    green_x : u32,
    green_y : u32,
    blue_x : u32,
    blue_y : u32,
}

gAMA : struct {
    gamma : u32,
}

iCCP : struct {
    profile_name : string,
    compression_method : u8 = 0,
    compressed_profile : u8[],
}

sBIT : struct {
    grayscale_bits : u8 if IHDR.color_type == ColorType.Grayscale,
    rgb_bits : RGB if IHDR.color_type == ColorType.Truecolor,
    alpha_bits : u8 if IHDR.color_type in {ColorType.GrayscaleAlpha, ColorType.TruecolorAlpha},
}

sRGB : struct {
    rendering_intent : RenderingIntent,
}

RenderingIntent : enum(u8) {
    Perceptual = 0,
    RelativeColorimetric = 1,
    Saturation = 2,
    AbsoluteColorimetric = 3,
}

tEXt : struct {
    keyword : string,
    text : string,
}

zTXt : struct {
    keyword : string,
    compression_method : u8 = 0,
    compressed_text : u8[],
}

iTXt : struct {
    compression_flag : u8,
    compression_method : u8 if compression_flag == 1,
    language_tag : string,
    translated_keyword : string,
    text : string,
}

bKGD : struct {
    grayscale : u16 if IHDR.color_type == ColorType.Grayscale,
    rgb : RGB if IHDR.color_type == ColorType.Truecolor,
    palette_index : u8 if IHDR.color_type == ColorType.IndexedColor,
}

pHYs : struct {
    pixels_per_unit_x : u32,
    pixels_per_unit_y : u32,
    unit_specifier : UnitSpecifier,
}

UnitSpecifier : enum(u8) {
    Unknown = 0,
    Meter = 1,
}

sPLT : struct {
    palette_name : string,
    sample_depth : u8,
    entries : list(sPLTEntry),
}

sPLTEntry : struct {
    r : u16,
    g : u16,
    b : u16,
    a : u16,
    frequency : u16,
}

hIST : struct {
    frequencies : list(u16),
}

tIME : struct {
    year : u16,
    month : u8,
    day : u8,
    hour : u8,
    minute : u8,
    second : u8,
}