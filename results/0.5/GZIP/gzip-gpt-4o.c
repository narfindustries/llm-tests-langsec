#include <hammer/hammer.h>

// Define the GZIP format structure
HParser *gzip_parser() {
    // Define the GZIP header fields
    HParser *id1 = h_byte(0x1f);
    HParser *id2 = h_byte(0x8b);
    HParser *compression_method = h_byte(0x08); // Deflate
    HParser *flags = h_bits(8, false);
    HParser *mtime = h_bits(32, false);
    HParser *xflags = h_bits(8, false);
    HParser *os = h_bits(8, false);

    // Define the GZIP header
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

    // Define the GZIP body (compressed data)
    HParser *gzip_body = h_until(h_eof(), h_bits(8, false));

    // Define the GZIP footer (CRC32 and ISIZE)
    HParser *crc32 = h_bits(32, false);
    HParser *isize = h_bits(32, false);

    // Define the GZIP structure
    HParser *gzip_structure = h_sequence(
        gzip_header,
        gzip_body,
        crc32,
        isize,
        NULL
    );

    return gzip_structure;
}

int main() {
    HParser *parser = gzip_parser();
    // Use the parser as needed
    h_parser_free(parser);
    return 0;
}