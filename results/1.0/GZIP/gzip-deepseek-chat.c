#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

// Define the GZIP header structure
HParser *gzip_header_parser() {
    return h_sequence(
        h_bytes(2, "ID1"),  // ID1: 0x1f
        h_bytes(2, "ID2"),  // ID2: 0x8b
        h_uint8(),          // Compression method
        h_uint8(),          // Flags
        h_uint32(),         // Modification time
        h_uint8(),          // Extra flags
        h_uint8(),          // Operating system
        NULL
    );
}

// Define the GZIP footer structure
HParser *gzip_footer_parser() {
    return h_sequence(
        h_uint32(),         // CRC32
        h_uint32(),         // Uncompressed size
        NULL
    );
}

// Define the GZIP parser
HParser *gzip_parser() {
    return h_sequence(
        gzip_header_parser(),
        h_many(h_uint8()),  // Compressed data
        gzip_footer_parser(),
        NULL
    );
}

// Main function to parse GZIP data
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gzip file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("GZIP parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("GZIP parsing failed!\n");
    }

    free(data);
    return 0;
}