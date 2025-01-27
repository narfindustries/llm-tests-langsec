let PNG_SIGNATURE = 0x89504E470D0A1A0A

struct PNGFile {
    signature: uint64 = PNG_SIGNATURE;
    chunks: Chunk[];
}

struct Chunk {
    length: uint32;
    type: uint32;
    data: uint8[length];
    crc: uint32;
}

struct IHDR {
    width: uint32;
    height: uint32;
    bit_depth: uint8;
    color_type: uint8;
    compression_method: uint8;
    filter_method: uint8;
    interlace_method: uint8;
}

struct PLTE {
    entries: RGB[length / 3];
}

struct RGB {
    red: uint8;
    green: uint8;
    blue: uint8;
}

struct IDAT {
    data: uint8[length];
}

struct IEND {
    // No data, just marks the end of the PNG file
}

struct Chunk {
    length: uint32;
    type: uint32;
    data: switch (type) {
        case 0x49484452: IHDR;
        case 0x504C5445: PLTE;
        case 0x49444154: IDAT;
        case 0x49454E44: IEND;
        default: uint8[length];
    };
    crc: uint32;
}