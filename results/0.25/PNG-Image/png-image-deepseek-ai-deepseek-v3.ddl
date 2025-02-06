PNGImage = struct {
    signature: bytes(8) where $ == b"\x89PNG\r\n\x1a\n";
    chunks: Chunk[];
};

Chunk = struct {
    length: uint32;
    type: bytes(4);
    data: switch (type) {
        case b"IHDR": IHDRChunk;
        case b"PLTE": PLTEChunk;
        case b"IDAT": IDATChunk;
        case b"IEND": IENDChunk;
        case b"tRNS": tRNSChunk;
        case b"gAMA": gAMAChunk;
        case b"cHRM": cHRMChunk;
        case b"sRGB": sRGBChunk;
        case b"iCCP": iCCPChunk;
        case b"tEXt": tEXtChunk;
        case b"zTXt": zTXtChunk;
        case b"iTXt": iTXtChunk;
        case b"bKGD": bKGDChunk;
        case b"hIST": hISTChunk;
        case b"pHYs": pHYsChunk;
        case b"sBIT": sBITChunk;
        case b"sPLT": sPLTChunk;
        case b"tIME": tIMEChunk;
        default: bytes(length);
    };
    crc: uint32;
};

IHDRChunk = struct {
    width: uint32;
    height: uint32;
    bit_depth: uint8 where $ in [1, 2, 4, 8, 16];
    color_type: uint8 where $ in [0, 2, 3, 4, 6];
    compression_method: uint8 where $ == 0;
    filter_method: uint8 where $ == 0;
    interlace_method: uint8 where $ in [0, 1];
};

PLTEChunk = struct {
    entries: RGB[length / 3];
};

RGB = struct {
    red: uint8;
    green: uint8;
    blue: uint8;
};

IDATChunk = struct {
    compressed_data: bytes(length);
};

IENDChunk = struct {
};

tRNSChunk = struct {
    transparency_data: switch (IHDRChunk.color_type) {
        case 0: GrayscaleTransparency;
        case 2: TruecolorTransparency;
        case 3: IndexedTransparency;
        default: bytes(length);
    };
};

GrayscaleTransparency = struct {
    gray: uint16;
};

TruecolorTransparency = struct {
    red: uint16;
    green: uint16;
    blue: uint16;
};

IndexedTransparency = struct {
    alpha: uint8[length];
};

gAMAChunk = struct {
    gamma: uint32;
};

cHRMChunk = struct {
    white_point_x: uint32;
    white_point_y: uint32;
    red_x: uint32;
    red_y: uint32;
    green_x: uint32;
    green_y: uint32;
    blue_x: uint32;
    blue_y: uint32;
};

sRGBChunk = struct {
    rendering_intent: uint8 where $ in [0, 1, 2, 3];
};

iCCPChunk = struct {
    profile_name: cstring;
    compression_method: uint8 where $ == 0;
    compressed_profile: bytes(length - len(profile_name) - 1);
};

tEXtChunk = struct {
    keyword: cstring;
    text: cstring;
};

zTXtChunk = struct {
    keyword: cstring;
    compression_method: uint8 where $ == 0;
    compressed_text: bytes(length - len(keyword) - 1);
};

iTXtChunk = struct {
    keyword: cstring;
    compression_flag: uint8 where $ in [0, 1];
    compression_method: uint8 where $ == 0;
    language_tag: cstring;
    translated_keyword: cstring;
    text: cstring;
};

bKGDChunk = struct {
    background_data: switch (IHDRChunk.color_type) {
        case 0: GrayscaleBackground;
        case 2: TruecolorBackground;
        case 3: IndexedBackground;
        default: bytes(length);
    };
};

GrayscaleBackground = struct {
    gray: uint16;
};

TruecolorBackground = struct {
    red: uint16;
    green: uint16;
    blue: uint16;
};

IndexedBackground = struct {
    index: uint8;
};

hISTChunk = struct {
    frequencies: uint16[length / 2];
};

pHYsChunk = struct {
    pixels_per_unit_x: uint32;
    pixels_per_unit_y: uint32;
    unit_specifier: uint8 where $ in [0, 1];
};

sBITChunk = struct {
    significant_bits: switch (IHDRChunk.color_type) {
        case 0: GrayscaleSignificantBits;
        case 2: TruecolorSignificantBits;
        case 3: IndexedSignificantBits;
        case 4: GrayscaleAlphaSignificantBits;
        case 6: TruecolorAlphaSignificantBits;
        default: bytes(length);
    };
};

GrayscaleSignificantBits = struct {
    gray: uint8;
};

TruecolorSignificantBits = struct {
    red: uint8;
    green: uint8;
    blue: uint8;
};

IndexedSignificantBits = struct {
    red: uint8;
    green: uint8;
    blue: uint8;
};

GrayscaleAlphaSignificantBits = struct {
    gray: uint8;
    alpha: uint8;
};

TruecolorAlphaSignificantBits = struct {
    red: uint8;
    green: uint8;
    blue: uint8;
    alpha: uint8;
};

sPLTChunk = struct {
    palette_name: cstring;
    sample_depth: uint8 where $ in [8, 16];
    entries: switch (sample_depth) {
        case 8: PaletteEntry8[length - len(palette_name) - 1];
        case 16: PaletteEntry16[length - len(palette_name) - 1];
    };
};

PaletteEntry8 = struct {
    red: uint8;
    green: uint8;
    blue: uint8;
    alpha: uint8;
};

PaletteEntry16 = struct {
    red: uint16;
    green: uint16;
    blue: uint16;
    alpha: uint16;
};

tIMEChunk = struct {
    year: uint16;
    month: uint8;
    day: uint8;
    hour: uint8;
    minute: uint8;
    second: uint8;
};