#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parser for GZIP file format
static HParser* gzip_parser(void) {
    // Magic header bytes for GZIP
    HParsedToken* magic_header = h_token("\x1F\x8B", 2);

    // Compression method (8 = DEFLATE)
    HParser* compression_method = h_token("\x08", 1);

    // Flags parser
    HParser* flags = h_bits(8, false);

    // Timestamp (4 bytes)
    HParser* timestamp = h_bits(32, false);

    // Extra flags
    HParser* extra_flags = h_bits(8, false);

    // Operating system
    HParser* os = h_bits(8, false);

    // Optional extra fields
    HParser* optional_extra = h_optional(
        h_sequence(
            h_bits(16, false),  // Extra length
            h_repeat_n(h_bits(8, false), h_int_range(0, 65535))  // Extra data
        )
    );

    // Optional filename
    HParser* filename = h_optional(
        h_many(h_not_char('\0'))
    );

    // Optional comment
    HParser* comment = h_optional(
        h_many(h_not_char('\0'))
    );

    // Compressed data block
    HParser* compressed_data = h_repeat_n(h_bits(8, false), h_int_range(0, INT_MAX));

    // CRC32 checksum
    HParser* crc32 = h_bits(32, false);

    // Uncompressed size
    HParser* uncompressed_size = h_bits(32, false);

    // Full GZIP file structure parser
    HParser* gzip_file = h_sequence(
        magic_header,
        compression_method,
        flags,
        timestamp,
        extra_flags,
        os,
        optional_extra,
        filename,
        comment,
        compressed_data,
        crc32,
        uncompressed_size
    );

    return gzip_file;
}

int main(int argc, char** argv) {
    // Initialize Hammer
    h_init();

    // Create GZIP parser
    HParser* parser = gzip_parser();

    // Check if file provided
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    // Read file
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
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    // Read file contents
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    // Parse GZIP file
    HParsedToken* result = h_parse(parser, buffer, read_size);

    // Check parsing result
    if (result) {
        printf("GZIP file parsed successfully\n");
        h_delete_parse_result(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    // Cleanup
    free(buffer);
    h_destroy(parser);

    return 0;
}