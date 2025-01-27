#include <hammer/hammer.h>

HParser *gzip_parser() {
    // Define basic parsers
    HParser *byte = h_uint8();
    HParser *word = h_uint16();
    HParser *dword = h_uint32();

    // Define fixed values
    HParser *gzip_id1 = h_byte(0x1F);
    HParser *gzip_id2 = h_byte(0x8B);
    HParser *compression_method = h_byte(0x08); // Deflate

    // Define flags
    HParser *flag = h_bits(8, false);

    // Define time, xflags, os
    HParser *mtime = dword;
    HParser *xflags = byte;
    HParser *os = byte;

    // Define optional fields based on flags
    HParser *extra_field = h_if(h_bit_test(flag, 2), h_length_value(word, byte));
    HParser *original_name = h_if(h_bit_test(flag, 3), h_until(byte, h_byte(0x00)));
    HParser *comment = h_if(h_bit_test(flag, 4), h_until(byte, h_byte(0x00)));
    HParser *header_crc = h_if(h_bit_test(flag, 1), word);

    // Define the gzip header
    HParser *gzip_header = h_sequence(
        gzip_id1,
        gzip_id2,
        compression_method,
        flag,
        mtime,
        xflags,
        os,
        extra_field,
        original_name,
        comment,
        header_crc,
        NULL
    );

    // Define the compressed data block
    HParser *compressed_data = h_until(byte, h_end_p());

    // Define the gzip footer
    HParser *gzip_footer = h_sequence(
        dword, // CRC32
        dword, // ISIZE
        NULL
    );

    // Define the full gzip parser
    HParser *gzip_file = h_sequence(
        gzip_header,
        compressed_data,
        gzip_footer,
        NULL
    );

    return gzip_file;
}