#include <hammer/hammer.h>

HParser *create_gzip_parser() {
    // GZIP Magic Number (0x1f 0x8b)
    HParser *gzip_magic = h_sequence(h_byte(0x1f), h_byte(0x8b), NULL);

    // Compression Method (8 for deflate)
    HParser *compression_method = h_byte(0x08);

    // Flags
    HParser *flags = h_bits(8, false);

    // Modification Time
    HParser *mtime = h_uint32();

    // Extra Flags
    HParser *xflags = h_bits(8, false);

    // Operating System
    HParser *os = h_bits(8, false);

    // Optional Extra Field
    HParser *extra_length = h_if(
        h_bits(1, false, 2), // Check the EXFLG bit in the flags
        h_uint16(),
        h_int_value(0)
    );

    HParser *extra = h_repeat(
        h_byte(1),
        h_puint16(extra_length, 0)
    );

    // Optional Original Filename
    HParser *original_filename = h_if(
        h_bits(1, false, 3), // Check the FNAME bit in the flags
        h_zstring(),
        h_empty()
    );

    // Optional Comment
    HParser *comment = h_if(
        h_bits(1, false, 4), // Check the FCOMMENT bit in the flags
        h_zstring(),
        h_empty()
    );

    // Optional CRC16 for Header
    HParser *header_crc16 = h_if(
        h_bits(1, false, 1), // Check the FHCRC bit in the flags
        h_uint16(),
        h_empty()
    );

    // Compressed Data
    HParser *compressed_data = h_until(
        h_uint32(), // Stop at first 4-byte value that matches CRC32
        h_byte(1)
    );

    // CRC32
    HParser *crc32 = h_uint32();

    // ISIZE (Input size modulo 2^32)
    HParser *isize = h_uint32();

    // Complete GZIP Parser
    return h_sequence(
        gzip_magic,
        compression_method,
        flags,
        mtime,
        xflags,
        os,
        extra,
        original_filename,
        comment,
        header_crc16,
        compressed_data,
        crc32,
        isize,
        NULL
    );
}