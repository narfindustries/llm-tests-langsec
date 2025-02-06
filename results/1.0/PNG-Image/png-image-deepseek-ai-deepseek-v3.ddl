PNGFile: {
    signature: b"\x89PNG\r\n\x1A\n",
    chunks: Chunk*,
};

Chunk: {
    length: uint32be,
    type: fixed_bytes(4),
    data: switch (type) {
        b"IHDR" => IHDR,
        b"PLTE" => PLTE,
        b"IDAT" => IDAT,
        b"IEND" => IEND,
        b"gAMA" => gAMA,
        b"cHRM" => cHRM,
        b"sRGB" => sRGB,
        b"iCCP" => iCCP,
        b"tEXt" => tEXt,
        b"zTXt" => zTXt,
        b"iTXt" => iTXt,
        b"bKGD" => bKGD,
        b"hIST" => hIST,
        b"pHYs" => pHYs,
        b"sBIT" => sBIT,
        b"tIME" => tIME,
        b"tRNS" => tRNS,
        _ => bytes(length),
    },
    crc: uint32be,
};

IHDR: {
    width: uint32be,
    height: uint32be,
    bit_depth: uint8,
    color_type: uint8,
    compression_method: uint8,
    filter_method: uint8,
    interlace_method: uint8,
};

PLTE: {
    entries: bgr*,
};

bgr: {
    red: uint8,
    green: uint8,
    blue: uint8,
};

IDAT: {
    compressed_data: bytes(length),
};

IEND: {};

gAMA: {
    gamma: uint32be,
};

cHRM: {
    white_x: uint32be,
    white_y: uint32be,
    red_x: uint32be,
    red_y: uint32be,
    green_x: uint32be,
    green_y: uint32be,
    blue_x: uint32be,
    blue_y: uint32be,
};

sRGB: {
    rendering_intent: uint8,
};

iCCP: {
    profile_name: null_terminated_ascii,
    compression_method: uint8,
    compressed_profile: bytes(length - len(profile_name) - 1),
};

tEXt: {
    keyword: null_terminated_ascii,
    text: null_terminated_ascii,
};

zTXt: {
    keyword: null_terminated_ascii,
    compression_method: uint8,
    compressed_text: bytes(length - len(keyword) - 1),
};

iTXt: {
    keyword: null_terminated_utf8,
    compression_flag: uint8,
    compression_method: uint8,
    language_tag: null_terminated_utf8,
    translated_keyword: null_terminated_utf8,
    text: null_terminated_utf8,
};

bKGD: switch (PNGFile.chunks.filter(c => c.type == b"IHDR")[0].color_type) {
    0 => {
        gray: uint16be,
    },
    2 => {
        red: uint16be,
        green: uint16be,
        blue: uint16be,
    },
    3 => {
        palette_index: uint8,
    },
};

hIST: {
    frequencies: uint16be*,
};

pHYs: {
    pixels_per_unit_x: uint32be,
    pixels_per_unit_y: uint32be,
    unit_specifier: uint8,
};

sBIT: switch (PNGFile.chunks.filter(c => c.type == b"IHDR")[0].color_type) {
    0 => {
        significant_gray_bits: uint8,
    },
    2 => {
        significant_red_bits: uint8,
        significant_green_bits: uint8,
        significant_blue_bits: uint8,
    },
    3 => {
        significant_red_bits: uint8,
        significant_green_bits: uint8,
        significant_blue_bits: uint8,
    },
    4 => {
        significant_gray_bits: uint8,
        significant_alpha_bits: uint8,
    },
    6 => {
        significant_red_bits: uint8,
        significant_green_bits: uint8,
        significant_blue_bits: uint8,
        significant_alpha_bits: uint8,
    },
};

tIME: {
    year: uint16be,
    month: uint8,
    day: uint8,
    hour: uint8,
    minute: uint8,
    second: uint8,
};

tRNS: switch (PNGFile.chunks.filter(c => c.type == b"IHDR")[0].color_type) {
    0 => {
        gray: uint16be,
    },
    2 => {
        red: uint16be,
        green: uint16be,
        blue: uint16be,
    },
    3 => {
        alpha: uint8*,
    },
};