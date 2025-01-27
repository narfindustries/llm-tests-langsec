#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the ZIP file structure parser
static HParser* zip_file_parser() {
    // Local file header signature (4 bytes)
    HParser* local_header_sig = h_token("\x50\x4B\x03\x04");

    // Version needed to extract (2 bytes)
    HParser* version_needed = h_uint16();

    // General purpose bit flag (2 bytes)
    HParser* bit_flag = h_uint16();

    // Compression method (2 bytes)
    HParser* compression_method = h_uint16();

    // Last mod file time (2 bytes)
    HParser* last_mod_time = h_uint16();

    // Last mod file date (2 bytes)
    HParser* last_mod_date = h_uint16();

    // CRC-32 (4 bytes)
    HParser* crc32 = h_uint32();

    // Compressed size (4 bytes)
    HParser* compressed_size = h_uint32();

    // Uncompressed size (4 bytes)
    HParser* uncompressed_size = h_uint32();

    // File name length (2 bytes)
    HParser* filename_length = h_uint16();

    // Extra field length (2 bytes)
    HParser* extra_field_length = h_uint16();

    // File name (variable length)
    HParser* filename = h_length_value(filename_length, h_ch());

    // Extra field (variable length)
    HParser* extra_field = h_length_value(extra_field_length, h_ch());

    // File data (variable length based on compressed size)
    HParser* file_data = h_length_value(compressed_size, h_ch());

    // Combine all parsers for local file header
    HParser* local_file_header = h_sequence(
        local_header_sig,
        version_needed,
        bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        filename,
        extra_field,
        file_data,
        NULL
    );

    return local_file_header;
}

int main(int argc, char** argv) {
    // Create the ZIP file parser
    HParser* zip_parser = zip_file_parser();

    // Check if a file was provided
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    // Read the file
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Allocate buffer
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    // Read file contents
    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    // Parse the ZIP file
    HParseResult* result = h_parse(zip_parser, buffer, file_size);

    // Check parsing result
    if (result && result->ast) {
        printf("ZIP file parsed successfully!\n");
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    // Cleanup
    free(buffer);
    h_parse_result_free(result);
    h_parser_free(zip_parser);

    return 0;
}