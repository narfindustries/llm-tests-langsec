module GZIP;

import "std/bytes";

struct GzipFile {
    header: GzipHeader;
    compressed_blocks: list<GzipCompressedBlock>;
    crc32: uint32;
    input_size: uint32;
}

struct GzipHeader {
    id1: uint8 == 0x1f;
    id2: uint8 == 0x8b;
    compression_method: uint8 == 0x08; // Deflate
    flags: GzipFlags;
    mtime: uint32;
    extra_flags: uint8;
    os: uint8;
    extra_field: optional<GzipExtraField> if (flags.fextra);
    filename: optional<string> if (flags.fname);
    comment: optional<string> if (flags.fcomment);
    hcrc: optional<uint16> if (flags.fhcrc);
}

struct GzipFlags {
    ftext: uint8 : 1;
    fextra: uint8 : 1;
    fname: uint8 : 1;
    fcomment: uint8 : 1;
    fhcrc: uint8 : 1;
    reserved: uint8 : 3;
}

struct GzipExtraField {
    xlen: uint16;
    subfields: bytes(xlen);
}

struct GzipCompressedBlock {
    data: bytes;
}