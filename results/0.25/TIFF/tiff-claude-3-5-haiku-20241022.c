#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define a parser for TIFF file structure
static HParser* tiff_parser() {
    // TIFF header parser
    HParser* header = h_sequence(
        h_literal("II"), // Little-endian identifier
        h_uint16(),      // Version number (42)
        h_uint32()       // First IFD offset
    );

    // IFD (Image File Directory) entry parser
    HParser* ifd_entry = h_sequence(
        h_uint16(),  // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32()  // Value/Offset
    );

    // Multiple IFD entries parser
    HParser* ifd_entries = h_many(ifd_entry);

    // Complete TIFF file parser
    HParser* tiff_file = h_sequence(
        header,
        ifd_entries
    );

    return tiff_file;
}

// Validation function
int validate_tiff(uint8_t* data, size_t len) {
    HParser* parser = tiff_parser();
    HParseResult* result = h_parse(parser, data, len);

    if (result && result->ast) {
        h_parse_result_free(result);
        return 1; // Valid TIFF
    }
    
    return 0; // Invalid TIFF
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        fprintf(stderr, "Error reading file\n");
        free(buffer);
        return 1;
    }

    int is_valid = validate_tiff(buffer, read_size);
    free(buffer);

    printf("TIFF file is %s\n", is_valid ? "valid" : "invalid");
    return is_valid ? 0 : 1;
}