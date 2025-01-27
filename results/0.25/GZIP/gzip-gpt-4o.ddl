GZIP = structure {
    header: GZIPHeader;
    compressed_data: bytes(header.compressed_size);
    crc32: uint32;
    isize: uint32;
}

GZIPHeader = structure {
    id1: uint8 { assert(id1 == 0x1f); }
    id2: uint8 { assert(id2 == 0x8b); }
    compression_method: uint8 { assert(compression_method == 0x08); }
    flags: uint8;
    mtime: uint32;
    extra_flags: uint8;
    os: uint8;
    extra: optional if (flags & 0x04) != 0 {
        length: uint16;
        data: bytes(length);
    }
    filename: optional if (flags & 0x08) != 0 {
        name: cstring;
    }
    comment: optional if (flags & 0x10) != 0 {
        text: cstring;
    }
    header_crc16: optional if (flags & 0x02) != 0 {
        crc16: uint16;
    }
    compressed_size: uint32;
}