struct PNGImage {
    magic: MagicNumber;
    ihdr: IHDRChunk;
    chunks: Chunk[];
}

struct MagicNumber {
    value: uint8[8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
}

struct IHDRChunk {
    length: uint32;
    type: uint8[4] = [0x49, 0x48, 0x44, 0x52];
    width: uint32;
    height: uint32;
    bit_depth: uint8;
    color_type: uint8;
    compression_method: uint8;
    filter_method: uint8;
    interlace_method: uint8;
    crc: uint32;
}

struct Chunk {
    length: uint32;
    type: uint8[4];
    data: uint8[length];
    crc: uint32;
}