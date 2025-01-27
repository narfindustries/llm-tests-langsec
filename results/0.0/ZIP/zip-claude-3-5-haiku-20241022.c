#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parser for ZIP file structure
static HParser* zip_file_parser() {
    // Magic number for ZIP files
    HParser* magic_number = h_literal_string("PK\x03\x04");

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

    // Filename length (2 bytes)
    HParser* filename_length = h_uint16();

    // Extra field length (2 bytes)
    HParser* extra_field_length = h_uint16();

    // Filename (variable length)
    HParser* filename = h_repeat_n(h_ch_range('A', 'z'), h_get_uint16(filename_length));

    // Extra field (variable length)
    HParser* extra_field = h_repeat_n(h_ch_any(), h_get_uint16(extra_field_length));

    // File data (variable length based on compressed size)
    HParser* file_data = h_repeat_n(h_ch_any(), h_get_uint32(compressed_size));

    // Combine all parsers
    HParser* zip_entry = h_sequence(
        magic_number,
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

    return zip_entry;
}

int main() {
    // Initialize Hammer
    h_init();

    // Create ZIP file parser
    HParser* zip_parser = zip_file_parser();

    // Example ZIP file data (simplified)
    const char* zip_data = "PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00test.txt";
    size_t zip_data_len = strlen(zip_data);

    // Parse ZIP file
    HParseResult* result = h_parse(zip_parser, (const uint8_t*)zip_data, zip_data_len);

    if (result && result->ast) {
        printf("ZIP file parsed successfully\n");
    } else {
        printf("ZIP file parsing failed\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_destroy();

    return 0;
}