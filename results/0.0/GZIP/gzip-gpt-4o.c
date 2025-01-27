#include <hammer/hammer.h>

HParser *gzip_parser() {
    // Define basic parsers
    HParser *byte = h_uint8();
    HParser *word = h_uint16();
    HParser *dword = h_uint32();

    // Define gzip header fields
    HParser *id1 = h_byte(0x1F);
    HParser *id2 = h_byte(0x8B);
    HParser *compression_method = h_byte(0x08); // Deflate
    HParser *flags = byte;
    HParser *mtime = dword;
    HParser *xflags = byte;
    HParser *os = byte;

    // Define gzip header
    HParser *gzip_header = h_sequence(
        id1,
        id2,
        compression_method,
        flags,
        mtime,
        xflags,
        os,
        NULL
    );

    // Define optional fields based on flags
    HParser *extra_field = h_if(
        h_bits_any(1, 1),
        h_length_value(word, byte),
        h_nothing()
    );

    HParser *original_filename = h_if(
        h_bits_any(2, 1),
        h_until(h_byte(0x00), byte),
        h_nothing()
    );

    HParser *comment = h_if(
        h_bits_any(4, 1),
        h_until(h_byte(0x00), byte),
        h_nothing()
    );

    HParser *header_crc16 = h_if(
        h_bits_any(8, 1),
        word,
        h_nothing()
    );

    // Define gzip body
    HParser *compressed_data = h_until(h_end_p(), byte);

    // Define gzip footer
    HParser *crc32 = dword;
    HParser *isize = dword;

    // Define full gzip parser
    HParser *gzip_parser = h_sequence(
        gzip_header,
        extra_field,
        original_filename,
        comment,
        header_crc16,
        compressed_data,
        crc32,
        isize,
        NULL
    );

    return gzip_parser;
}