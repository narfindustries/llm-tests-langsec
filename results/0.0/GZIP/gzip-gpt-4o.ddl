GZIP : STRUCTURE := {
    id1 : UINT8 { assert = $ = 0x1F },
    id2 : UINT8 { assert = $ = 0x8B },
    cm : UINT8 { assert = $ = 0x08 }, // Compression Method: DEFLATE
    flg : BITFIELD UINT8 := {
        ftext : 1,
        fhcrc : 1,
        fextra : 1,
        fname : 1,
        fcomment : 1,
        reserved : 3 { assert = $ = 0 }
    },
    mtime : UINT32, // Modification Time
    xfl : UINT8, // Extra Flags
    os : UINT8, // Operating System
    extra_field : OPTIONAL STRUCTURE := IF(flg.fextra = 1) {
        extra_length : UINT16,
        extra_data : SEQUENCE OF UINT8(extra_length)
    },
    original_file_name : OPTIONAL STRING := IF(flg.fname = 1) {
        name : STRING(ASCII, TERMINATED BY 0x00)
    },
    file_comment : OPTIONAL STRING := IF(flg.fcomment = 1) {
        comment : STRING(ASCII, TERMINATED BY 0x00)
    },
    header_crc16 : OPTIONAL UINT16 := IF(flg.fhcrc = 1) {
        crc16 : UINT16
    },
    compressed_data : SEQUENCE OF UINT8, // Compressed Data
    crc32 : UINT32, // CRC32 of uncompressed data
    isize : UINT32 // Size of uncompressed data modulo 2^32
}