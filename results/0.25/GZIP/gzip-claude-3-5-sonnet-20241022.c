#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_gzip_parser() {
    // Fixed length fields
    HParser* id1 = h_uint8();
    HParser* id2 = h_uint8();
    HParser* cm = h_uint8();
    HParser* flg = h_uint8();
    HParser* mtime = h_uint32();
    HParser* xfl = h_uint8();
    HParser* os = h_uint8();

    // Optional fields based on flags
    HParser* xlen = h_uint16();
    HParser* extra = h_many(h_uint8());
    HParser* fname = h_many1(h_not_in((const uint8_t*)"\0", 1));
    HParser* fname_term = h_ch('\0');
    HParser* fcomment = h_many1(h_not_in((const uint8_t*)"\0", 1));
    HParser* fcomment_term = h_ch('\0');
    HParser* crc16 = h_uint16();

    // Compressed data and footer
    HParser* compressed_data = h_many1(h_uint8());
    HParser* crc32 = h_uint32();
    HParser* isize = h_uint32();

    // Build sequence for optional fields
    HParser* optional_fields = h_sequence(xlen, extra, fname, fname_term, 
                                        fcomment, fcomment_term, crc16, NULL);

    // Complete GZIP format
    return h_sequence(id1, id2, cm, flg, mtime, xfl, os, 
                     h_optional(optional_fields),
                     compressed_data, crc32, isize, NULL);
}

const HParser* gzip_parser = init_gzip_parser();

const HParserBackend* backend = &h_packrat_backend;

int main(int argc, char** argv) {
    uint8_t input[] = {
        0x1f, 0x8b, 0x08, 0x00,  // ID1, ID2, CM, FLG
        0x00, 0x00, 0x00, 0x00,  // MTIME
        0x00,                     // XFL
        0x03,                     // OS
        0x48, 0x65, 0x6c, 0x6c,  // Compressed data "Hello"
        0x6f, 0x20, 0x57, 0x6f,
        0x72, 0x6c, 0x64, 0x00,
        0x00, 0x00, 0x00, 0x00,  // CRC32
        0x0c, 0x00, 0x00, 0x00   // ISIZE
    };

    HParseResult* result = h_parse(gzip_parser, input, sizeof(input));
    
    if(result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
        return 0;
    } else {
        printf("Parsing failed\n");
        return 1;
    }
}