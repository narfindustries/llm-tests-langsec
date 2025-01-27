format GZIP {
    let magic_number = bytes([0x1F, 0x8B]);
    let compression_method = uint8;
    let flags = uint8;
    let mtime = uint32_le;
    let extra_flags = uint8;
    let os = uint8;

    let optional_extra_header = optional {
        let si1 = uint8;
        let si2 = uint8;
        let extra_length = uint16_le;
        let extra_data = bytes(extra_length);
    };

    let optional_filename = optional_string;
    let optional_comment = optional_string;
    let optional_header_crc = optional { uint16_le };

    let compressed_data = bytes;
    let crc32 = uint32_le;
    let input_size = uint32_le;

    record GZIP {
        magic_number: bytes(2),
        compression_method: uint8,
        flags: uint8,
        mtime: uint32,
        extra_flags: uint8,
        os: uint8,
        extra_header: optional_extra_header if (flags & 0x04) != 0,
        filename: optional_filename if (flags & 0x08) != 0,
        comment: optional_comment if (flags & 0x10) != 0,
        header_crc: optional_header_crc if (flags & 0x02) != 0,
        compressed_data: bytes,
        crc32: uint32,
        input_size: uint32
    }

    parse = { 
        magic_number: magic_number,
        compression_method: compression_method,
        flags: flags,
        mtime: mtime,
        extra_flags: extra_flags,
        os: os,
        extra_header: if (flags & 0x04) != 0 then optional_extra_header else null,
        filename: if (flags & 0x08) != 0 then optional_filename else null,
        comment: if (flags & 0x10) != 0 then optional_comment else null,
        header_crc: if (flags & 0x02) != 0 then optional_header_crc else null,
        compressed_data: compressed_data,
        crc32: crc32,
        input_size: input_size
    }
}