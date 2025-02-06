type gzip_file = record {
    magic: [0x1F, 0x8B],
    compression_method: u8 where value == 0x08,
    flags: bit_flags,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_fields: optional(extra_field),
    filename: optional(filename),
    comment: optional(comment),
    header_crc: optional(u16),
    compressed_data: bytes,
    crc32: u32,
    original_length: u32
}

type bit_flags = record {
    text_hint: bool,
    header_crc: bool,
    extra_field: bool,
    filename: bool,
    comment: bool,
    reserved1: bool,
    reserved2: bool
}

type extra_field = record {
    length: u16,
    data: bytes[length]
}

type filename = array(u8) while not (last == 0x00)

type comment = array(u8) while not (last == 0x00)