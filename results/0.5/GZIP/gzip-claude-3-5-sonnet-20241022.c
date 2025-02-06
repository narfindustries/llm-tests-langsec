#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser* init_gzip_parser() {
    // Fixed ID fields
    HParser* id1 = h_int_range(h_bits(8, false), 0x1F, 0x1F);
    HParser* id2 = h_int_range(h_bits(8, false), 0x8B, 0x8B);
    
    // Compression method
    HParser* cm = h_int_range(h_bits(8, false), 8, 8);
    
    // Flags
    HParser* flags = h_bits(8, false);
    
    // MTIME (4 bytes)
    HParser* mtime = h_bits(32, false);
    
    // Extra flags
    HParser* xfl = h_bits(8, false);
    
    // Operating system
    HParser* os = h_bits(8, false);

    // Optional fields parsers
    HParser* extra_length = h_bits(16, false);
    HParser* extra_content = h_many1(h_uint8());
    HParser* extra_field = h_sequence(extra_length, extra_content, NULL);
    
    HParser* filename = h_many1(h_choice(h_not_in("\0", 1), NULL));
    HParser* filename_term = h_sequence(filename, h_ch('\0'), NULL);
    
    HParser* comment = h_many1(h_choice(h_not_in("\0", 1), NULL));
    HParser* comment_term = h_sequence(comment, h_ch('\0'), NULL);
    
    HParser* header_crc = h_bits(16, false);

    // Create optional field sequence based on flags
    HParser* optional_fields = h_sequence(
        h_optional(extra_field),    // FEXTRA
        h_optional(filename_term),  // FNAME
        h_optional(comment_term),   // FCOMMENT
        h_optional(header_crc),     // FHCRC
        NULL
    );

    // Compressed data (variable length)
    HParser* compressed_data = h_many1(h_uint8());

    // Trailing fields
    HParser* crc32 = h_bits(32, false);
    HParser* isize = h_bits(32, false);

    // Combine all parsers
    return h_sequence(
        id1,
        id2,
        cm,
        flags,
        mtime,
        xfl,
        os,
        optional_fields,
        compressed_data,
        crc32,
        isize,
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        fclose(f);
        free(input);
        return 1;
    }
    fclose(f);

    HParser* gzip_parser = init_gzip_parser();
    HParseResult* result = h_parse(gzip_parser, input, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(input);
        return 1;
    }

    h_parse_result_free(result);
    free(input);
    return 0;
}