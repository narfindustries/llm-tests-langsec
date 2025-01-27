format GZIP {
    let magic_number = bytes([0x1F, 0x8B]);
    let compression_method = 8; // Deflate
    let flags = {
        text: 0b00000001,
        header_crc: 0b00000010,
        extra_fields: 0b00000100,
        original_filename: 0b00001000,
        comment: 0b00010000
    };

    type Header {
        magic: magic_number,
        compression: u8 = compression_method,
        flag_byte: u8,
        modification_time: u32,
        compression_flags: u8,
        operating_system: u8
    }

    type ExtraField {
        id: u16,
        length: u16,
        data: bytes(length)
    }

    type Filename {
        name: string(null_terminated)
    }

    type Comment {
        text: string(null_terminated)
    }

    type Trailer {
        crc32: u32,
        input_size: u32
    }

    type GZIPFile {
        header: Header,
        extra_fields: optional(flags.extra_fields, ExtraField),
        filename: optional(flags.original_filename, Filename),
        comment: optional(flags.comment, Comment),
        header_crc: optional(flags.header_crc, u16),
        compressed_data: bytes,
        trailer: Trailer
    }

    parse: GZIPFile
}