typedef PNG = struct {
    signature: bytes[8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
    chunks: Chunk[]
}

typedef Chunk = struct {
    length: u32
    type: bytes[4]
    data: ChunkData
    crc: u32
}

typedef ChunkData = union {
    IHDR: struct {
        width: u32
        height: u32
        bit_depth: u8
        color_type: u8
        compression_method: u8
        filter_method: u8
        interlace_method: u8
    }
    PLTE: struct {
        entries: struct {
            r: u8
            g: u8
            b: u8
        }[]
    }
    IDAT: struct {
        data: bytes
    }
    IEND: struct {}
    tRNS: struct {
        data: bytes
    }
    cHRM: struct {
        white_point_x: u32
        white_point_y: u32
        red_x: u32
        red_y: u32
        green_x: u32
        green_y: u32
        blue_x: u32
        blue_y: u32
    }
    gAMA: struct {
        gamma: u32
    }
    iCCP: struct {
        profile_name: string
        compression_method: u8
        compressed_profile: bytes
    }
    sBIT: struct {
        significant_bits: bytes[4]
    }
    sRGB: struct {
        rendering_intent: u8
    }
    tEXt: struct {
        keyword: string
        text: bytes
    }
    zTXt: struct {
        keyword: string
        compression_method: u8
        compressed_text: bytes
    }
    iTXt: struct {
        keyword: string
        compression_flag: u8
        compression_method: u8
        language_tag: string
        translated_keyword: string
        text: bytes
    }
    bKGD: struct {
        background_color: bytes[6]
    }
    hIST: struct {
        frequencies: u16[]
    }
    pHYs: struct {
        pixels_per_unit_x: u32
        pixels_per_unit_y: u32
        unit_specifier: u8
    }
    sPLT: struct {
        palette_name: string
        sample_depth: u8
        entries: bytes
    }
    tIME: struct {
        year: u16
        month: u8
        day: u8
        hour: u8
        minute: u8
        second: u8
    }
}