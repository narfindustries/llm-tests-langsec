#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_gzip_parser(void) {
    // GZIP header fields
    HParser* id1 = h_ch(0x1f);
    HParser* id2 = h_ch(0x8b);
    HParser* cm = h_ch(0x08);
    HParser* flg = h_uint8();
    HParser* mtime = h_uint32();
    HParser* xfl = h_uint8();
    HParser* os = h_uint8();

    // Optional fields based on flags
    HParser* xlen = h_uint16();
    HParser* extra = h_many1(h_uint8());
    HParser* fname = h_many1(h_ch_range(0x00, 0x7f));
    HParser* fname_term = h_ch(0x00);
    HParser* fcomment = h_many1(h_ch_range(0x00, 0x7f));
    HParser* fcomment_term = h_ch(0x00);
    HParser* crc16 = h_uint16();

    // Compressed data and footer
    HParser* compressed_data = h_many1(h_uint8());
    HParser* crc32 = h_uint32();
    HParser* isize = h_uint32();

    // Build the full parser
    return h_sequence(id1, id2, cm, flg, mtime, xfl, os,
                     h_optional(h_sequence(xlen, extra, NULL)),
                     h_optional(h_sequence(fname, fname_term, NULL)),
                     h_optional(h_sequence(fcomment, fcomment_term, NULL)),
                     h_optional(crc16),
                     compressed_data,
                     crc32, isize,
                     NULL);
}

int main(int argc, char** argv) {
    HParser* parser = init_gzip_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}