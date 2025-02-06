#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function to handle errors
void handle_error(const char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    exit(EXIT_FAILURE);
}

// Parser Definitions
HParser* png_signature() {
    return h_bits(64, false);
}

HParser* png_chunk() {
    HParser* length = h_uint32();
    HParser* type = h_bits(32, false);
    HParser* data = h_length_value(length, h_any());  // h_any reads as many bytes as specified by length
    HParser* crc = h_uint32();
    return h_sequence(length, type, data, crc, NULL);
}

HParser* png_file() {
    HParser* signature = png_signature();
    HParser* chunks = h_many(png_chunk());
    return h_sequence(signature, chunks, NULL);
}

int parse_png(const char* filename) {
    // Open file
    FILE* file = fopen(filename, "rb");
    if (!file) {
        handle_error("Could not open file");
    }

    // Seek to the end of the file to get the file size
    fseek(file, 0, SEEK_END);
    size_t filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file contents into a buffer
    uint8_t* buffer = malloc(filesize);
    if (!buffer) {
        handle_error("Memory allocation failed");
    }
    fread(buffer, 1, filesize, file);

    // Close the file
    fclose(file);

    // Parse the PNG
    HParseResult* result = h_parse(png_file(), buffer, filesize);
    if (result == NULL) {
        handle_error("Parsing failed");
    } else {
        printf("PNG parsed successfully.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    free(buffer);

    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    return parse_png(argv[1]);
}