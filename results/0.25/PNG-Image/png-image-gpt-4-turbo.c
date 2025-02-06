#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define PNG signature
static const uint8_t png_signature[] = {0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n'};

// Parser for a single chunk
HParser *png_chunk() {
    HParser *chunk_length = h_uint32();
    HParser *chunk_type = h_bits(32, false);
    HParser *chunk_data = h_length_value(h_uint32(), h_bits(0, false));
    HParser *chunk_crc = h_uint32();
    return h_sequence(chunk_length, chunk_type, chunk_data, chunk_crc, NULL);
}

// Parser for PNG file
HParser *png_parser() {
    HParser *signature = h_bits(64, false);
    HParser *chunks = h_many(png_chunk());
    return h_sequence(signature, chunks, NULL);
}

// Main function
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    // Open PNG file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file into memory
    uint8_t *data = malloc(file_size);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }
    fread(data, 1, file_size, file);

    // Create PNG parser
    HParser *parser = png_parser();

    // Parse the data
    HParseResult *result = h_parse(parser, data, file_size);
    if (result) {
        printf("PNG file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse PNG file.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_parser_unref(parser);
    free(data);
    fclose(file);

    return 0;
}