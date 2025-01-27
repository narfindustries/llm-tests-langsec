#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// Define ZIP file structure parser
static HParser* zip_parser(void) {
    // File signature
    HParser* signature = h_token("\x50\x4B\x03\x04", 4);

    // Version needed to extract 
    HParser* version = h_uint16();

    // General purpose bit flag
    HParser* flags = h_uint16();

    // Compression method 
    HParser* compression = h_uint16();

    // Last mod time
    HParser* mod_time = h_uint16();

    // Last mod date
    HParser* mod_date = h_uint16();

    // CRC-32 checksum
    HParser* crc32 = h_uint32();

    // Compressed size
    HParser* compressed_size = h_uint32();

    // Uncompressed size
    HParser* uncompressed_size = h_uint32(); 

    // Filename length
    HParser* filename_len = h_uint16();

    // Extra field length
    HParser* extra_len = h_uint16();

    // Filename 
    HParser* filename = h_repeat_upto(h_ch_range('A', 'Z'), 0, 256);

    // Extra field (optional)
    HParser* extra_field = h_repeat_upto(h_any_char(), 0, 256);

    // File data 
    HParser* file_data = h_repeat_upto(h_any_char(), 0, 65535);

    // Combine parsers into complete ZIP local file header
    HParser* zip_local_header = h_sequence(
        signature,     // 0: File signature 
        version,       // 1: Version 
        flags,         // 2: Flags
        compression,   // 3: Compression 
        mod_time,      // 4: Modification time
        mod_date,      // 5: Modification date 
        crc32,         // 6: CRC checksum
        compressed_size, // 7: Compressed size
        uncompressed_size, // 8: Uncompressed size
        filename_len,  // 9: Filename length
        extra_len,     // 10: Extra field length
        filename,      // 11: Filename 
        extra_field,   // 12: Extra field
        file_data,     // 13: File data
        NULL
    );

    return zip_local_header;
}

int main(int argc, char** argv) {
    // Initialize Hammer 
    h_init(NULL, NULL);

    // Create ZIP parser
    HParser* parser = zip_parser();

    // Example ZIP file data (simplified)
    const char zip_data[] = {
        0x50, 0x4B, 0x03, 0x04,  // Signature
        0x14, 0x00,              // Version
        0x00, 0x00,              // Flags
        0x00, 0x00,              // Compression method
        0x00, 0x00,              // Last mod time
        0x00, 0x00,              // Last mod date
        0x00, 0x00, 0x00, 0x00,  // CRC-32
        0x00, 0x00, 0x00, 0x00,  // Compressed size
        0x00, 0x00, 0x00, 0x00,  // Uncompressed size
        0x04, 0x00,              // Filename length
        0x00, 0x00,              // Extra field length
        'T', 'E', 'S', 'T'       // Filename
    };

    // Parse ZIP file
    HParseResult* result = h_parse(parser, (const uint8_t*)zip_data, sizeof(zip_data));

    // Cleanup
    h_parse_result_free(result);
    h_destroy(parser);

    return 0;
}