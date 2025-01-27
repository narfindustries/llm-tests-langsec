#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parser for gzip file format
static HParser* gzip_file_parser() {
    // Magic header bytes for gzip
    HParser* magic_header = h_literal_bytes("\x1F\x8B", 2);

    // Compression method (8 = DEFLATE)
    HParser* compression_method = h_ch(8);

    // Flags byte
    HParser* flags = h_bits(8, false);

    // Modification time (4 bytes)
    HParser* mod_time = h_bits(32, false);

    // Extra flags
    HParser* extra_flags = h_bits(8, false);

    // Operating system identifier
    HParser* os_id = h_bits(8, false);

    // Optional extra headers
    HParser* optional_extra = h_optional(
        h_sequence(
            h_bits(16, false),  // Extra length
            h_repeat_n(h_bits(8, false), h_get_bits_val(h_bits(16, false)))
        )
    );

    // Optional filename
    HParser* optional_filename = h_optional(
        h_many(h_not_ch(0))
    );

    // Optional comment
    HParser* optional_comment = h_optional(
        h_many(h_not_ch(0))
    );

    // Optional header CRC
    HParser* optional_header_crc = h_optional(
        h_bits(16, false)
    );

    // Compressed data block
    HParser* compressed_data = h_many(h_bits(8, false));

    // CRC32 checksum
    HParser* crc32 = h_bits(32, false);

    // Uncompressed size
    HParser* uncompressed_size = h_bits(32, false);

    // Full gzip file structure
    return h_sequence(
        magic_header,
        compression_method,
        flags,
        mod_time,
        extra_flags,
        os_id,
        optional_extra,
        optional_filename,
        optional_comment,
        optional_header_crc,
        compressed_data,
        crc32,
        uncompressed_size
    );
}

int main(int argc, char** argv) {
    // Initialize Hammer
    hammer_init();

    // Create parser
    HParser* parser = gzip_file_parser();

    // Parse input file if provided
    if (argc > 1) {
        FILE* input_file = fopen(argv[1], "rb");
        if (!input_file) {
            perror("Error opening file");
            return 1;
        }

        // Get file size
        fseek(input_file, 0, SEEK_END);
        long file_size = ftell(input_file);
        rewind(input_file);

        // Read file contents
        uint8_t* buffer = malloc(file_size);
        if (!buffer) {
            perror("Memory allocation error");
            fclose(input_file);
            return 1;
        }

        size_t bytes_read = fread(buffer, 1, file_size, input_file);
        fclose(input_file);

        if (bytes_read != file_size) {
            perror("File read error");
            free(buffer);
            return 1;
        }

        // Parse input
        HParseResult* result = h_parse(parser, buffer, bytes_read);
        if (result) {
            printf("Parsing successful\n");
            h_parse_result_free(result);
        } else {
            printf("Parsing failed\n");
        }

        free(buffer);
    }

    // Cleanup
    h_parser_free(parser);
    hammer_shutdown();

    return 0;
}