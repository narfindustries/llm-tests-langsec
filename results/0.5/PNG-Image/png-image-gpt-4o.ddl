RootStruct PNG {
    header: PNGSignature;
    chunks: Chunk[] (until: _.type == "IEND");
}

Struct PNGSignature {
    signature: u8[8] (assert: _ == b'\x89PNG\r\n\x1a\n');
}

Struct Chunk {
    length: u32be;
    type: AsciiStr[4];
    data: u8[length];
    crc: u32be;
    switch (type) {
        case "IHDR": ihdr: IHDR;
        case "PLTE": plte: PLTE;
        case "IDAT": idat: IDAT;
        case "IEND": iend: IEND;
        case "tRNS": trns: tRNS;
        case "cHRM": chrm: cHRM;
        case "gAMA": gama: gAMA;
        case "iCCP": iccp: iCCP;
        case "sBIT": sbit: sBIT;
        case "sRGB": srgb: sRGB;
        case "tEXt": text: tEXt;
        case "zTXt": ztxt: zTXt;
        case "iTXt": itxt: iTXt;
        case "bKGD": bkgd: bKGD;
        case "pHYs": phys: pHYs;
        case "sPLT": splt: sPLT;
        case "hIST": hist: hIST;
        case "tIME": time: tIME;
    }
}

Struct IHDR {
    width: u32be;
    height: u32be;
    bit_depth: u8;
    color_type: u8;
    compression_method: u8 (assert: _ == 0);
    filter_method: u8 (assert: _ == 0);
    interlace_method: u8;
}

Struct PLTE {
    entries: RGB[(length / 3)];
}

Struct IDAT {
    data: u8[length];
}

Struct IEND {
    // No data
}

Struct tRNS {
    data: u8[length];
}

Struct cHRM {
    white_point_x: u32be;
    white_point_y: u32be;
    red_x: u32be;
    red_y: u32be;
    green_x: u32be;
    green_y: u32be;
    blue_x: u32be;
    blue_y: u32be;
}

Struct gAMA {
    gamma: u32be;
}

Struct iCCP {
    profile_name: AsciiStr (until: _ == 0);
    compression_method: u8;
    compressed_profile: u8[length - profile_name.length - 2];
}

Struct sBIT {
    significant_bits: u8[length];
}

Struct sRGB {
    rendering_intent: u8;
}

Struct tEXt {
    keyword: AsciiStr (until: _ == 0);
    text: AsciiStr[length - keyword.length - 1];
}

Struct zTXt {
    keyword: AsciiStr (until: _ == 0);
    compression_method: u8;
    compressed_text: u8[length - keyword.length - 2];
}

Struct iTXt {
    compressed: u8;
    compression_method: u8;
    language_tag: AsciiStr (until: _ == 0);
    translated_keyword: Utf8Str (until: _ == 0);
    text: Utf8Str[length - language_tag.length - translated_keyword.length - 3];
}

Struct bKGD {
    data: u8[length];
}

Struct pHYs {
    pixels_per_unit_x: u32be;
    pixels_per_unit_y: u32be;
    unit_specifier: u8;
}

Struct sPLT {
    palette_name: AsciiStr (until: _ == 0);
    sample_depth: u8;
    entries: PaletteEntry[(length - palette_name.length - 2) / (sample_depth == 8 ? 6 : 10)];
}

Struct PaletteEntry {
    red: u16be (if: sample_depth == 16) | u8;
    green: u16be (if: sample_depth == 16) | u8;
    blue: u16be (if: sample_depth == 16) | u8;
    alpha: u16be (if: sample_depth == 16) | u8;
    frequency: u16be;
}

Struct hIST {
    frequencies: u16be[length / 2];
}

Struct tIME {
    year: u16be;
    month: u8;
    day: u8;
    hour: u8;
    minute: u8;
    second: u8;
}

Struct RGB {
    red: u8;
    green: u8;
    blue: u8;
}