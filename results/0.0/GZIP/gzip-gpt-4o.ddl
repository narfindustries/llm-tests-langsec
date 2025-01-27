// Daedalus specification for GZIP format

@format("gzip")
struct Gzip {
    Header header;
    CompressedData compressedData;
    Footer footer;
}

struct Header {
    uint8 id1; // ID1 should be 0x1F
    uint8 id2; // ID2 should be 0x8B
    uint8 compressionMethod; // Compression method (8 = deflate)
    Flags flags;
    uint32 mtime; // Modification time
    uint8 extraFlags; // Extra flags
    uint8 os; // Operating system
    if (flags.fextra) {
        ExtraField extraField;
    }
    if (flags.fname) {
        string fileName;
    }
    if (flags.fcomment) {
        string comment;
    }
    if (flags.fhcrc) {
        uint16 headerCrc16;
    }
}

struct Flags {
    uint8 reserved:3;
    uint8 fhcrc:1; // Header CRC16 present
    uint8 fextra:1; // Extra field present
    uint8 fname:1; // Original file name present
    uint8 fcomment:1; // File comment present
    uint8 ftext:1; // File is ASCII text
}

struct ExtraField {
    uint16 xlen; // Length of the extra field
    uint8[xlen] data; // Extra field data
}

struct CompressedData {
    uint8[] data; // Compressed data
}

struct Footer {
    uint32 crc32; // CRC32 of the uncompressed data
    uint32 isize; // Input size modulo 2^32
}