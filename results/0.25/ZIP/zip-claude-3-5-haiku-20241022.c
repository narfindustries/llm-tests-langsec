#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define ZIP file structure parsing
static HParser* zip_signature;
static HParser* zip_version;
static HParser* zip_flags;
static HParser* zip_compression;
static HParser* zip_last_mod_time;
static HParser* zip_last_mod_date;
static HParser* zip_crc32;
static HParser* zip_compressed_size;
static HParser* zip_uncompressed_size;
static HParser* zip_filename_length;
static HParser* zip_extra_field_length;
static HParser* zip_filename;
static HParser* zip_extra_field;
static HParser* zip_local_file_header;

// Initialize parsers
void init_zip_parsers() {
    // PK signature
    zip_signature = h_literal_string("PK\x03\x04");

    // Version needed to extract (2 bytes)
    zip_version = h_uint16();

    // General purpose bit flag (2 bytes)
    zip_flags = h_uint16();

    // Compression method (2 bytes)
    zip_compression = h_uint16();

    // Last modified time (2 bytes)
    zip_last_mod_time = h_uint16();

    // Last modified date (2 bytes)
    zip_last_mod_date = h_uint16();

    // CRC-32 (4 bytes)
    zip_crc32 = h_uint32();

    // Compressed size (4 bytes)
    zip_compressed_size = h_uint32();

    // Uncompressed size (4 bytes)
    zip_uncompressed_size = h_uint32();

    // Filename length (2 bytes)
    zip_filename_length = h_uint16();

    // Extra field length (2 bytes)
    zip_extra_field_length = h_uint16();

    // Filename (variable length)
    zip_filename = h_length_value(zip_filename_length, h_char());

    // Extra field (variable length)
    zip_extra_field = h_length_value(zip_extra_field_length, h_char());

    // Complete local file header parser
    zip_local_file_header = h_sequence(
        zip_signature,
        zip_version,
        zip_flags,
        zip_compression,
        zip_last_mod_time,
        zip_last_mod_date,
        zip_crc32,
        zip_compressed_size,
        zip_uncompressed_size,
        zip_filename_length,
        zip_extra_field_length,
        zip_filename,
        zip_extra_field,
        NULL
    );
}

int main(int argc, char** argv) {
    // Initialize Hammer
    h_init();

    // Create parser
    init_zip_parsers();

    // Test parsing
    const char* test_zip_data = "PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00hello";
    HParseResult* result = h_parse(zip_local_file_header, test_zip_data, strlen(test_zip_data));

    if (result && result->ast) {
        printf("ZIP file parsed successfully\n");
    } else {
        printf("ZIP file parsing failed\n");
    }

    // Cleanup
    h_destroy();
    return 0;
}