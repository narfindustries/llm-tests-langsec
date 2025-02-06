def PNG = {
    signature: bytes(8) where self == [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A],
    chunks: list(Chunk)
}

def Chunk = {
    length: u32,
    type: ChunkType,
    data: match type {
        ChunkType.IHDR => IHDR,
        ChunkType.PLTE => PLTE,
        ChunkType.tRNS => Transparency,
        ChunkType.gAMA => Gamma,
        ChunkType.cHRM => Chromaticity,
        ChunkType.sRGB => StandardRGB,
        ChunkType.iCCP => ICCProfile,
        ChunkType.tEXt => TextChunk,
        ChunkType.zTXt => CompressedTextChunk,
        ChunkType.iTXt => InternationalTextChunk,
        ChunkType.bKGD => BackgroundColor,
        ChunkType.pHYs => PhysicalPixelDimensions,
        ChunkType.sBIT => SignificantBits,
        ChunkType.tIME => LastModificationTime,
        ChunkType.IDAT => ImageData,
        ChunkType.IEND => EndChunk
    },
    crc: u32
}

def IHDR = {
    width: u32 where value >= 1 and value <= 0x7FFFFFFF,
    height: u32 where value >= 1 and value <= 0x7FFFFFFF,
    bit_depth: u8 where value in [1, 2, 4, 8, 16],
    color_type: u8 where value in [0, 2, 3, 4, 6],
    compression_method: u8 where value == 0,
    filter_method: u8 where value == 0,
    interlace_method: u8 where value in [0, 1]
}

def PLTE = list(RGB, max_size: 256)

def RGB = {
    red: u8,
    green: u8,
    blue: u8
}

def Transparency = match color_type {
    0 => { gray_value: u16 },
    2 => { red: u16, green: u16, blue: u16 },
    3 => list(u8)
}

def Gamma = {
    gamma_value: u32
}

def Chromaticity = {
    white_x: u32,
    white_y: u32,
    red_x: u32,
    red_y: u32,
    green_x: u32,
    green_y: u32,
    blue_x: u32,
    blue_y: u32
}

def StandardRGB = {
    rendering_intent: u8 where value in [0, 1, 2, 3]
}

def ICCProfile = {
    profile_name: string,
    compression_method: u8,
    compressed_profile: bytes
}

def TextChunk = {
    keyword: string,
    text: string
}

def CompressedTextChunk = {
    keyword: string,
    compression_method: u8,
    compressed_text: bytes
}

def InternationalTextChunk = {
    keyword: string,
    compression_flag: u8,
    compression_method: u8,
    language_tag: string,
    translated_keyword: string,
    text: string
}

def BackgroundColor = match color_type {
    0 => { gray_value: u16 },
    2 => { red: u16, green: u16, blue: u16 },
    3 => { palette_index: u8 }
}

def PhysicalPixelDimensions = {
    pixels_per_x_unit: u32,
    pixels_per_y_unit: u32,
    unit_specifier: u8 where value in [0, 1]
}

def SignificantBits = match color_type {
    0 => { gray_bits: u8 },
    2 => { red_bits: u8, green_bits: u8, blue_bits: u8 },
    3 => { red_bits: u8, green_bits: u8, blue_bits: u8 },
    4 => { gray_bits: u8, alpha_bits: u8 },
    6 => { red_bits: u8, green_bits: u8, blue_bits: u8, alpha_bits: u8 }
}

def LastModificationTime = {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8
}

def ImageData = bytes

def EndChunk = {}

enum ChunkType {
    IHDR = 0x49484452,
    PLTE = 0x504C5445,
    tRNS = 0x74524E53,
    gAMA = 0x67414D41,
    cHRM = 0x6348524D,
    sRGB = 0x73524742,
    iCCP = 0x69434350,
    tEXt = 0x74455874,
    zTXt = 0x7A545874,
    iTXt = 0x69545874,
    bKGD = 0x624B4744,
    pHYs = 0x70485973,
    sBIT = 0x73424954,
    tIME = 0x74494D45,
    IDAT = 0x49444154,
    IEND = 0x49454E44
}