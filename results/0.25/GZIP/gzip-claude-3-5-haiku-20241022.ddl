type GzipFile = {
    magic: [2]u8 where magic[0] == 0x1F and magic[1] == 0x8B,
    compression_method: u8 where value == 0x08,
    flags: {
        text: u1,
        hcrc: u1,
        extra: u1,
        name: u1,
        comment: u1,
        encrypted: u1,
        reserved: [2]u1
    },
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_field: if flags.extra == 1 then {
        length: u16,
        data: [length]u8
    } else unit,
    original_filename: if flags.name == 1 then {
        chars: [*]u8 while current != 0,
        terminator: u8 where value == 0
    } else unit,
    file_comment: if flags.comment == 1 then {
        chars: [*]u8 while current != 0,
        terminator: u8 where value == 0
    } else unit,
    header_crc: if flags.hcrc == 1 then u16 else unit,
    compressed_data: opaque,
    crc32: u32,
    original_length: u32
}