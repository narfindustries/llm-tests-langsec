@format("PNG")
struct PngImage {
    signature: Magic;
    chunks: Chunk[];
}

struct Magic {
    @display("PNG Signature")
    value: u64 @equals(0x89504E470D0A1A0A);
}

struct Chunk {
    length: u32;
    type: ChunkType;
    data: u8[length];
    crc: u32;
}

enum ChunkType: u32 {
    IHDR = 0x49484452,
    PLTE = 0x504C5445,
    IDAT = 0x49444154,
    IEND = 0x49454E44,
    // Add other chunk types as needed
}

struct IHDRChunk {
    width: u32;
    height: u32;
    bitDepth: u8;
    colorType: u8;
    compressionMethod: u8;
    filterMethod: u8;
    interlaceMethod: u8;
}

struct PLTEChunk {
    entries: RgbEntry[];
}

struct RgbEntry {
    red: u8;
    green: u8;
    blue: u8;
}

struct IDATChunk {
    compressedData: u8[];
}

struct IENDChunk {
    // No data for IEND chunk
}