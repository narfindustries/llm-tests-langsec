type gzip_header = struct {
    id1: u8
        where { self == 0x1F };
    id2: u8
        where { self == 0x8B };
    compression_method: u8
        where { self == 0x08 };
    flags: bitfield {
        text_flag: 1;
        header_crc_flag: 1;
        extra_flag: 1;
        filename_flag: 1;
        comment_flag: 1;
        reserved: 3;
    };
    modification_time: u32;
    extra_flags: u8;
    operating_system: u8;
    extra_field: if (flags.extra_flag == 1) then {
        length: u16;
        data: bytes(length);
    } else unit;
    original_filename: if (flags.filename_flag == 1) then 
        null_terminated_string 
    else unit;
    file_comment: if (flags.comment_flag == 1) then 
        null_terminated_string 
    else unit;
    header_crc: if (flags.header_crc_flag == 1) then u16 else unit;
    compressed_data: bytes;
    crc32: u32;
    original_length: u32;
}