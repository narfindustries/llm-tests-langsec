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

struct GZIPFileName {
    name: uint8[] { stop: 0x00 };
}

struct GZIPComment {
    comment: uint8[] { stop: 0x00 };
}

struct GZIPCRC16 {
    crc16: uint16;
}

struct GZIP {
    header: GZIPHeader;
    extra_field: GZIPExtraField if header.flags & 0x04;
    file_name: GZIPFileName if header.flags & 0x08;
    comment: GZIPComment if header.flags & 0x10;
    crc16: GZIPCRC16 if header.flags & 0x02;
    compressed_data: uint8[] { stop: eof };
    crc32: uint32;
    uncompressed_size: uint32;
}