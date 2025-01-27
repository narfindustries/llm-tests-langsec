#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the parser for the GZIP format
static HParser* gzip_parser(void) {
    // Magic number parser
    HParser* magic_number = h_token_bytes(
        (uint8_t*)"\x1F\x8B", 2
    );

    // Compression method parser (8 = DEFLATE)
    HParser* compression_method = h_bits_be(8, false);

    // Flags parser
    HParser* flags = h_bits_be(8, false);

    // Modification time (4 bytes, little-endian)
    HParser* mod_time = h_bits_le(32, false);

    // Extra flags parser
    HParser* extra_flags = h_bits_be(8, false);

    // Operating system parser
    HParser* os_type = h_bits_be(8, false);

    // Optional extra headers parser
    HParser* extra_header = h_optional(
        h_sequence(
            h_bits_be(16, false),  // Extra length
            h_repeat_n(h_bits_be(8, false), 
                       h_indirect_parser(extra_header), 1)
        )
    );

    // Optional filename parser
    HParser* filename = h_optional(
        h_zero_or_more(h_not_char('\0'))
    );

    // Optional comment parser
    HParser* comment = h_optional(
        h_zero_or_more(h_not_char('\0'))
    );

    // CRC16 for header
    HParser* header_crc16 = h_bits_le(16, false);

    // Compressed data block parser (simplified)
    HParser* compressed_data = h_repeat_n(
        h_bits_be(8, false), 
        h_indirect_parser(compressed_data), 
        1
    );

    // CRC32 for compressed data
    HParser* data_crc32 = h_bits_le(32, false);

    // Original uncompressed data length
    HParser* original_length = h_bits_le(32, false);

    // Complete GZIP file parser
    HParser* gzip_file = h_sequence(
        magic_number,
        compression_method,
        flags,
        mod_time,
        extra_flags,
        os_type,
        h_optional(extra_header),
        h_optional(filename),
        h_optional(comment),
        h_optional(header_crc16),
        compressed_data,
        data_crc32,
        original_length
    );

    return gzip_file;
}

int main(int argc, char** argv) {
    // Initialize Hammer
    hammer_init();

    // Create the parser
    HParser* parser = gzip_parser();

    // If input file provided, parse it
    if (argc > 1) {
        FILE* file = fopen(argv[1], "rb");
        if (!file) {
            perror("Error opening file");
            return 1;
        }

        // Read file contents
        fseek(file, 0, SEEK_END);
        long file_size = ftell(file);
        fseek(file, 0, SEEK_SET);

        uint8_t* buffer = malloc(file_size);
        if (!buffer) {
            perror("Memory allocation error");
            fclose(file);
            return 1;
        }

        fread(buffer, 1, file_size, file);
        fclose(file);

        // Parse the file
        HParseResult* result = h_parse(parser, buffer, file_size);
        
        if (result) {
            printf("Parsing successful!\n");
            h_parse_result_free(result);
        } else {
            printf("Parsing failed.\n");
        }

        free(buffer);
    }

    // Clean up parser
    h_parse_destroy(parser);

    return 0;
}