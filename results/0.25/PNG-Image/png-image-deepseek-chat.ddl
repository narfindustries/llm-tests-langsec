let PNG_SIGNATURE = 0x89504E470D0A1A0A

struct PNGFile {
    signature: uint64 = PNG_SIGNATURE;
    chunks: Chunk[] until eof;
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
        case "IHDR": IHDR;
        case "PLTE": PLTE;
        case "IDAT": IDAT;
        case "IEND": IEND;
        default: uint8[length];
    };
    crc: uint32;
}