#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser *gzip_header;
static HParser *gzip_optional_fields;
static HParser *gzip_footer;
static HParser *gzip_parser;

static HParsedToken *act_make_gzip(const HParseResult *p, void *user_data) {
    return (HParsedToken *)p->ast;
}

void init_parser(void) {
    // Fixed ID fields
    HParser *id1 = h_ch('\x1f');
    HParser *id2 = h_ch('\x8b');
    
    // Compression Method
    HParser *cm = h_ch('\x08');  // Only deflate is supported
    
    // Flags
    HParser *flags = h_uint8();
    
    // Basic header
    HParser *basic_header = h_sequence(id1, id2, cm, flags, NULL);
    
    // MTIME (4 bytes)
    HParser *mtime = h_uint32();
    
    // XFL (1 byte)
    HParser *xfl = h_uint8();
    
    // OS (1 byte)
    HParser *os = h_uint8();
    
    // Optional fields based on flags
    // FEXTRA
    HParser *xlen = h_uint16();
    HParser *extra_field = h_length_value(xlen, h_uint8());
    HParser *fextra = h_optional(h_sequence(xlen, extra_field, NULL));
    
    // FNAME
    HParser *not_null = h_not_in("\0", 1);
    HParser *fname = h_optional(h_sequence(h_many(not_null), h_ch('\0'), NULL));
    
    // FCOMMENT
    HParser *fcomment = h_optional(h_sequence(h_many(not_null), h_ch('\0'), NULL));
    
    // FHCRC
    HParser *hcrc = h_optional(h_uint16());
    
    // Compressed data (variable length)
    HParser *compressed_data = h_many1(h_uint8());
    
    // Footer
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();
    gzip_footer = h_sequence(crc32, isize, NULL);
    
    // Combine all parts
    gzip_parser = h_sequence(basic_header,
                           mtime,
                           xfl,
                           os,
                           fextra,
                           fname,
                           fcomment,
                           hcrc,
                           compressed_data,
                           gzip_footer,
                           NULL);
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

    // Get file size
    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    // Read entire file
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

    // Initialize parser
    init_parser();

    // Parse input
    HParseResult *result = h_parse(gzip_parser, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(input);
        return 1;
    }

    // Result is available in result->ast
    // Add processing code here if needed

    h_parse_result_free(result);
    free(input);
    return 0;
}