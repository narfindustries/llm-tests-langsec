struct PNGImage {
    magic: MagicNumber;
    ihdr: IHDRChunk;
    chunks: Chunk[];
}

struct MagicNumber {
    value: uint8[8] = [137, 80, 78, 71, 13, 10, 26, 10];
}

struct IHDRChunk {
    length: uint32;
    type: uint8[4] = [73, 72, 68, 82];
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