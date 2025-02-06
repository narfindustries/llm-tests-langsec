type gzip_file {
    magic: [u8; 2] = [0x1F, 0x8B]

    compression_method: u8 = 0x08

    flags: u8

    flag_text: bool = (flags & 0x01) != 0
    flag_hcrc: bool = (flags & 0x02) != 0
    flag_extra: bool = (flags & 0x04) != 0
    flag_name: bool = (flags & 0x08) != 0
    flag_comment: bool = (flags & 0x10) != 0

    mtime: u32

    extra_flags: u8 where {
        this == 0 or this == 2 or this == 4
    }

    os: u8 where {
        this <= 13 or this == 255
    }

    optional_extra: [u8] if flag_extra {
        length: u16
        data: [u8; length]
    }

    optional_filename: string if flag_name {
        value: string(null_terminated)
    }

    optional_comment: string if flag_comment {
        value: string(null_terminated)
    }

    optional_header_crc: u16 if flag_hcrc

    compressed_data: [u8]

    footer_crc32: u32
    footer_size: u32
}