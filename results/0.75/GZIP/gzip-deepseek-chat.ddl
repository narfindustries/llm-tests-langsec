struct GZIP {
    magic: uint8[2] == [0x1f, 0x8b];
    compression_method: uint8;
    flags: uint8;
    modification_time: uint32;
    extra_flags: uint8;
    os_type: uint8;
    optional_fields: OptionalFields;
    compressed_data: uint8[];
    crc32: uint32;
    uncompressed_size: uint32;
}

struct OptionalFields {
    ftext: bool if flags & 0x01;
    fhcrc: bool if flags & 0x02;
    fextra: ExtraField if flags & 0x04;
    fname: String if flags & 0x08;
    fcomment: String if flags & 0x10;
}

struct ExtraField {
    length: uint16;
    data: uint8[length];
}

struct String {
    length: uint16;
    data: uint8[length];
}