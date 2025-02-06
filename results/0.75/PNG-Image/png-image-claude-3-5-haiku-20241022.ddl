def PngSignature = bytes(8) where this == "\x89PNG\r\n\x1A\n"

type ChunkType = enum {
    IHDR,
    PLTE,
    tRNS,
    gAMA,
    cHRM,
    sRGB,
    iCCP,
    tEXt,
    zTXt,
    iTXt,
    bKGD,
    pHYs,
    sBIT,
    sPLT,
    hIST,
    tIME,
    IDAT,
    IEND
}

struct ImageHeader {
    width: u32,
    height: u32,
    bitDepth: u8 where [1, 2, 4, 8, 16],
    colorType: u8 where [0, 2, 3, 4, 6],
    compressionMethod: u8 where [0],
    filterMethod: u8 where [0],
    interlaceMethod: u8 where [0, 1]
}

struct Chunk {
    length: u32,
    type: ChunkType,
    data: bytes(length),
    crc: u32
}

struct PngImage {
    signature: PngSignature,
    chunks: [Chunk] where chunks[chunks.length - 1].type == ChunkType.IEND
}

def parse = PngImage