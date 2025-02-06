#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Signature constants
static const uint32_t LOCAL_FILE_HEADER_SIG = 0x04034b50;
static const uint32_t CENTRAL_DIR_SIG = 0x02014b50;
static const uint32_t END_CENTRAL_DIR_SIG = 0x06054b50;

// Parser declarations
HParser *zip_parser;
HParser *local_file_header;
HParser *central_directory;
HParser *end_central_directory;
HParser *data_descriptor;
HParser *extra_field;

// Helper parsers
static HParser* uint16_le() {
    return h_uint16();
}

static HParser* uint32_le() {
    return h_uint32();
}

static HParser* variable_length_string(HParser* length_parser) {
    return h_length_value(length_parser, h_uint8());
}

void init_parsers() {
    // Extra field parser
    extra_field = h_sequence(
        uint16_le(),  // Header ID
        h_length_value(uint16_le(), h_uint8()),  // Data
        NULL
    );

    // Data descriptor parser (optional)
    data_descriptor = h_sequence(
        uint32_le(),  // CRC-32
        uint32_le(),  // Compressed size
        uint32_le(),  // Uncompressed size
        NULL
    );

    // Local file header parser
    local_file_header = h_sequence(
        h_token((const uint8_t*)&LOCAL_FILE_HEADER_SIG, sizeof(uint32_t)),
        uint16_le(),  // Version needed
        uint16_le(),  // General purpose flag
        uint16_le(),  // Compression method
        uint16_le(),  // Last mod time
        uint16_le(),  // Last mod date
        uint32_le(),  // CRC-32
        uint32_le(),  // Compressed size
        uint32_le(),  // Uncompressed size
        variable_length_string(uint16_le()),  // Filename
        h_optional(h_length_value(uint16_le(), extra_field)),  // Extra field
        h_optional(data_descriptor),  // Optional data descriptor
        NULL
    );

    // Central directory entry parser
    central_directory = h_sequence(
        h_token((const uint8_t*)&CENTRAL_DIR_SIG, sizeof(uint32_t)),
        uint16_le(),  // Version made by
        uint16_le(),  // Version needed
        uint16_le(),  // General purpose flag
        uint16_le(),  // Compression method
        uint16_le(),  // Last mod time
        uint16_le(),  // Last mod date
        uint32_le(),  // CRC-32
        uint32_le(),  // Compressed size
        uint32_le(),  // Uncompressed size
        variable_length_string(uint16_le()),  // Filename
        h_optional(h_length_value(uint16_le(), extra_field)),  // Extra field
        variable_length_string(uint16_le()),  // File comment
        uint16_le(),  // Disk number start
        uint16_le(),  // Internal attributes
        uint32_le(),  // External attributes
        uint32_le(),  // Local header offset
        NULL
    );

    // End of central directory parser
    end_central_directory = h_sequence(
        h_token((const uint8_t*)&END_CENTRAL_DIR_SIG, sizeof(uint32_t)),
        uint16_le(),  // Disk number
        uint16_le(),  // Start disk number
        uint16_le(),  // Number of central directory records on disk
        uint16_le(),  // Total number of central directory records
        uint32_le(),  // Size of central directory
        uint32_le(),  // Offset of central directory
        variable_length_string(uint16_le()),  // ZIP comment
        NULL
    );

    // Complete ZIP file parser
    zip_parser = h_sequence(
        h_many(local_file_header),
        h_many(central_directory),
        end_central_directory,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
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

    init_parsers();

    HParseResult *result = (HParseResult*)h_parse(zip_parser, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(input);
        fclose(f);
        return 1;
    }

    printf("Successfully parsed ZIP file\n");

    h_parse_result_free(result);
    free(input);
    fclose(f);
    return 0;
}