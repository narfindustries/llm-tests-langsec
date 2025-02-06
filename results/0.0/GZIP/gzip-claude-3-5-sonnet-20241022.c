#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParsedToken* check_flag_action(const HParseResult* p, void* user_data) {
    return h_make_uint(1);
}

HParser* init_gzip_parser() {
    // Fixed magic numbers
    HParser* id1 = h_ch(0x1F);
    HParser* id2 = h_ch(0x8B);
    
    // Compression method (8 for DEFLATE)
    HParser* cm = h_uint8();
    
    // Flags
    HParser* flg = h_bits(8, false);
    
    // MTIME (4 bytes)
    HParser* mtime = h_uint32();
    
    // Extra flags
    HParser* xfl = h_uint8();
    
    // Operating system
    HParser* os = h_uint8();
    
    // Optional XLEN and extra field
    HParser* xlen = h_uint16();
    HParser* extra_subfield = h_sequence(h_uint16(), h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser* extra = h_length_value(xlen, extra_subfield);
    
    // Optional filename (null-terminated)
    HParser* filename = h_token((const uint8_t*)"", 0);
    
    // Optional comment (null-terminated)
    HParser* comment = h_token((const uint8_t*)"", 0);
    
    // Optional header CRC16
    HParser* hcrc16 = h_uint16();
    
    // Create sequence based on flags
    HParser* optional_fields = h_indirect();
    
    h_bind_indirect(optional_fields,
        h_sequence(
            h_optional(h_action(extra, check_flag_action, NULL)),     // FEXTRA
            h_optional(h_action(filename, check_flag_action, NULL)),  // FNAME
            h_optional(h_action(comment, check_flag_action, NULL)),   // FCOMMENT
            h_optional(h_action(hcrc16, check_flag_action, NULL)),    // FHCRC
            NULL
        )
    );
    
    // Compressed data and trailing fields
    HParser* crc32 = h_uint32();
    HParser* isize = h_uint32();
    
    // Compressed blocks (remaining data)
    HParser* compressed_data = h_many1(h_uint8());
    
    // Complete GZIP format
    return h_sequence(
        id1, id2, cm, flg, mtime, xfl, os,
        optional_fields,
        compressed_data,
        crc32, isize,
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* input = malloc(size);
    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        fclose(file);
        free(input);
        return 1;
    }
    fclose(file);
    
    HParser* gzip_parser = init_gzip_parser();
    
    HParseResult* result = h_parse(gzip_parser, input, size);
    
    if (result) {
        printf("Successfully parsed GZIP file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GZIP file\n");
    }
    
    free(input);
    return 0;
}