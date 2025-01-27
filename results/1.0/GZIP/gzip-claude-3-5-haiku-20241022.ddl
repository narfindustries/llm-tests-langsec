format GZIP:
    let file_header = {
        signature: bytes[2] == [0x1F, 0x8B],
        compression_method: u8,
        flags: u8,
        modification_time: u32,
        extra_flags: u8,
        operating_system: u8
    }

    let optional_extra_fields = match flags:
        when flags & 0x04 == 0x04:
            let extra_length = u16
            bytes[extra_length]
        else:
            []

    let optional_filename = match flags:
        when flags & 0x08 == 0x08:
            null_terminated_string
        else:
            []

    let optional_comment = match flags:
        when flags & 0x10 == 0x10:
            null_terminated_string
        else:
            []

    let optional_header_crc = match flags:
        when flags & 0x02 == 0x02:
            u16
        else:
            []

    let compressed_blocks = deflate_compressed_data

    let file_footer = {
        crc32: u32,
        input_size: u32
    }

    structure: 
        file_header
        optional_extra_fields
        optional_filename
        optional_comment
        optional_header_crc
        compressed_blocks
        file_footer