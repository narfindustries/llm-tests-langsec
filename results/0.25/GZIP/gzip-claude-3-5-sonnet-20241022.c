#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser* init_gzip_parser() {
    // Fixed magic numbers
    HParser* id1 = h_int_range(h_bits(8, false), 0x1F, 0x1F);
    HParser* id2 = h_int_range(h_bits(8, false), 0x8B, 0x8B);
    
    // Compression method (8 = DEFLATE)
    HParser* cm = h_int_range(h_bits(8, false), 0, 15);
    
    // Flags
    HParser* flg = h_bits(8, false);
    
    // MTIME (4 bytes)
    HParser* mtime = h_bits(32, false);
    
    // Extra flags
    HParser* xfl = h_bits(8, false);
    
    // Operating system
    HParser* os = h_bits(8, false);

    // Optional fields parsers
    // XLEN and EXTRA field
    HParser* xlen = h_bits(16, false);
    HParser* extra_subfield = h_sequence(h_bits(16, false),  // SI1, SI2
                                       h_bits(16, false),    // LEN
                                       h_length_value(h_bits(16, false), h_uint8()),
                                       NULL);
    
    // FNAME (null-terminated string)
    HParser* fname = h_token((const uint8_t*)"", 0);
    
    // FCOMMENT (null-terminated string)
    HParser* fcomment = h_token((const uint8_t*)"", 0);
    
    // HCRC16
    HParser* hcrc16 = h_bits(16, false);
    
    // Compressed data (variable length)
    HParser* compressed_data = h_many(h_uint8());
    
    // Trailing fields
    HParser* crc32 = h_bits(32, false);
    HParser* isize = h_bits(32, false);

    // Combine all parsers based on flags
    HParser* gzip = h_sequence(id1, id2, cm, flg, mtime, xfl, os,
                             h_optional(h_action(flg, h_bits(4, false), NULL)),  // FEXTRA
                             h_optional(h_action(flg, h_bits(8, false), NULL)),  // FNAME
                             h_optional(h_action(flg, h_bits(16, false), NULL)), // FCOMMENT
                             h_optional(h_action(flg, h_bits(2, false), NULL)),  // FHCRC
                             compressed_data,
                             crc32, isize,
                             NULL);
    
    return gzip;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }
    
    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser *gzip_parser = init_gzip_parser();
    HParseResult *result = h_parse(gzip_parser, input, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(input);
        return 1;
    }

    h_parse_result_free(result);
    free(input);
    return 0;
}