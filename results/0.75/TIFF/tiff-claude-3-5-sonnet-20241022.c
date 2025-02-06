#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* init_tiff_parser(void);

// Helper parsers
static const HParser* byte_order_parser(void) {
    return h_choice(h_token((uint8_t*)"II", 2), 
                    h_token((uint8_t*)"MM", 2), 
                    NULL);
}

static const HParser* version_parser(void) {
    return h_int_range(h_uint16(), 42, 42);
}

static const HParser* tag_type_parser(void) {
    return h_int_range(h_uint16(), 1, 12);
}

static const HParser* compression_parser(void) {
    return h_choice(h_int_range(h_uint16(), 1, 6),
                   h_int_range(h_uint16(), 32773, 32773),
                   NULL);
}

static const HParser* photometric_interpretation_parser(void) {
    return h_int_range(h_uint16(), 0, 8);
}

static const HParser* resolution_unit_parser(void) {
    return h_int_range(h_uint16(), 1, 3);
}

static const HParser* planar_configuration_parser(void) {
    return h_int_range(h_uint16(), 1, 2);
}

static const HParser* orientation_parser(void) {
    return h_int_range(h_uint16(), 1, 8);
}

static const HParser* extra_samples_parser(void) {
    return h_int_range(h_uint16(), 0, 2);
}

static const HParser* ifd_entry_parser(void) {
    return h_sequence(h_uint16(),           // Tag
                     tag_type_parser(),     // Type
                     h_uint32(),            // Count
                     h_uint32(),            // Value/Offset
                     NULL);
}

static const HParser* ifd_parser(void) {
    return h_sequence(h_uint16(),           // Number of entries
                     h_many(ifd_entry_parser()),
                     h_uint32(),            // Next IFD offset
                     NULL);
}

static const HParser* tiff_header_parser(void) {
    return h_sequence(byte_order_parser(),
                     version_parser(),
                     h_uint32(),            // First IFD offset
                     NULL);
}

HParser* init_tiff_parser(void) {
    return h_sequence(tiff_header_parser(),
                     ifd_parser(),
                     NULL);
}

void print_parse_result(HParsedToken* result) {
    if (!result) {
        printf("Failed to parse TIFF file\n");
        return;
    }
    
    // Add result printing logic here
    printf("Successfully parsed TIFF file\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read entire file
    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Initialize parser
    HParser *parser = init_tiff_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Parse the buffer
    HParseResult *result = h_parse(parser, buffer, size);
    
    if (result) {
        print_parse_result(result->ast);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TIFF file\n");
    }

    free(buffer);
    fclose(fp);
    return 0;
}