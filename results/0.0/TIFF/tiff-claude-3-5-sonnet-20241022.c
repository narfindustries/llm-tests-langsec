#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// TIFF Parser using Hammer

// Helper parsers for common structures
HParser* byte_order_parser() {
    return h_choice(h_token((uint8_t*)"II", 2),  // little-endian
                    h_token((uint8_t*)"MM", 2),   // big-endian
                    NULL);
}

HParser* version_parser() {
    return h_int_range(h_uint16(), 42, 43);  // 42 for regular TIFF, 43 for BigTIFF
}

HParser* ifd_entry_parser() {
    return h_sequence(
        h_uint16(),  // Tag
        h_uint16(),  // Type
        h_uint32(),  // Count
        h_uint32(),  // Value/Offset
        NULL
    );
}

HParser* ifd_parser() {
    return h_sequence(
        h_uint16(),              // Number of directory entries
        h_many1(ifd_entry_parser()),  // Directory entries
        h_uint32(),              // Offset to next IFD
        NULL
    );
}

HParser* tiff_parser() {
    return h_sequence(
        byte_order_parser(),    // Byte order
        version_parser(),       // Version
        h_uint32(),            // Offset to first IFD
        h_many1(ifd_parser()), // IFDs
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    // Read file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        perror("Failed to read file");
        free(data);
        fclose(f);
        return 1;
    }
    fclose(f);

    // Parse TIFF
    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, data, size);

    if (!result) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        free(data);
        return 1;
    }

    // Result is available in result->ast
    // Add processing logic here as needed

    h_parse_result_free(result);
    free(data);
    return 0;
}