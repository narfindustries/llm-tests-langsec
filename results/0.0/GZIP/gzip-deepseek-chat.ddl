struct GZIPHeader {
    magic: uint8[2] = [0x1f, 0x8b];
    compression_method: uint8;
    flags: uint8;
    modification_time: uint32;
    extra_flags: uint8;
    os_type: uint8;
}

struct GZIPExtraField {
    length: uint16;
    data: uint8[length];
}

struct GZIPFile {
    header: GZIPHeader;
    extra_field: GZIPExtraField if header.flags & 0x04 != 0;
    original_filename: cstring if header.flags & 0x08 != 0;
    comment: cstring if header.flags & 0x10 != 0;
    crc16: uint16 if header.flags & 0x02 != 0;
    compressed_data: uint8[];
    crc32: uint32;
    original_size: uint32;
}