#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// PNG Signature
#define PNG_SIG_SIZE 8
const uint8_t png_signature[PNG_SIG_SIZE] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

// Function prototypes
void parse_png(const char *filename);
void check_png_signature(const uint8_t *signature);
void handle_chunk(const uint8_t *chunk_data, uint32_t length, const uint8_t *type);

// PNG Chunk structure
typedef struct {
    uint8_t length[4];
    uint8_t type[4];
    uint8_t *data;
    uint8_t crc[4];
} PngChunk;

// PNG Parser
void parse_png(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    uint8_t signature[PNG_SIG_SIZE];
    fread(signature, 1, PNG_SIG_SIZE, file);
    check_png_signature(signature);

    while (!feof(file)) {
        PngChunk chunk;
        fread(chunk.length, 1, 4, file);
        fread(chunk.type, 1, 4, file);

        uint32_t length = (chunk.length[0] << 24) | (chunk.length[1] << 16) |
                          (chunk.length[2] << 8) | chunk.length[3];

        chunk.data = (uint8_t *)malloc(length);
        fread(chunk.data, 1, length, file);
        fread(chunk.crc, 1, 4, file);

        handle_chunk(chunk.data, length, chunk.type);
        free(chunk.data);

        if (feof(file)) break;
    }

    fclose(file);
}

// Check PNG signature
void check_png_signature(const uint8_t *signature) {
    for (int i = 0; i < PNG_SIG_SIZE; i++) {
        if (signature[i] != png_signature[i]) {
            fprintf(stderr, "Invalid PNG signature\n");
            exit(EXIT_FAILURE);
        }
    }
}

// Handle individual PNG chunk
void handle_chunk(const uint8_t *chunk_data, uint32_t length, const uint8_t *type) {
    if (strncmp((const char *)type, "IHDR", 4) == 0) {
        printf("IHDR chunk found\n");
        // Handle IHDR chunk
    } else if (strncmp((const char *)type, "IEND", 4) == 0) {
        printf("IEND chunk found\n");
        // Handle IEND chunk
    } else if (strncmp((const char *)type, "IDAT", 4) == 0) {
        printf("IDAT chunk found\n");
        // Handle IDAT chunk
    } else if (strncmp((const char *)type, "PLTE", 4) == 0) {
        printf("PLTE chunk found\n");
        // Handle PLTE chunk
    } else {
        printf("Unknown chunk: %c%c%c%c\n", type[0], type[1], type[2], type[3]);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_png(argv[1]);

    return EXIT_SUCCESS;
}