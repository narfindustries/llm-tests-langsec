#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the PNG file structure
typedef struct {
    uint8_t signature[8];
    uint32_t length;
    uint32_t chunk_type;
    uint8_t *data;
    uint32_t crc;
} PNGChunk;

// Define the PNG file structure
typedef struct {
    uint8_t signature[8];
    PNGChunk *chunks;
    size_t num_chunks;
} PNGFile;

// Parser for the PNG signature
HParser *png_signature_parser() {
    return h_sequence(
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

// Parser for a PNG chunk
HParser *png_chunk_parser() {
    return h_sequence(
        h_length_value(h_uint32(), h_many(h_uint8())), // Length and Data
        h_uint32(), // Chunk Type
        h_uint32(), // CRC
        NULL
    );
}

// Parser for the entire PNG file
HParser *png_file_parser() {
    return h_sequence(
        png_signature_parser(),
        h_many(png_chunk_parser()),
        NULL
    );
}

// Function to parse a PNG file
PNGFile *parse_png_file(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(png_file_parser(), data, length);
    if (!result) {
        fprintf(stderr, "Failed to parse PNG file\n");
        return NULL;
    }

    PNGFile *png_file = malloc(sizeof(PNGFile));
    if (!png_file) {
        fprintf(stderr, "Failed to allocate memory for PNG file\n");
        h_free_parse_result(result);
        return NULL;
    }

    // Extract the signature
    for (int i = 0; i < 8; i++) {
        png_file->signature[i] = ((uint8_t *)result->ast->seq->elements[0]->seq->elements[i]->uint8)[0];
    }

    // Extract the chunks
    png_file->num_chunks = result->ast->seq->elements[1]->seq->used;
    png_file->chunks = malloc(png_file->num_chunks * sizeof(PNGChunk));
    if (!png_file->chunks) {
        fprintf(stderr, "Failed to allocate memory for PNG chunks\n");
        h_free_parse_result(result);
        free(png_file);
        return NULL;
    }

    for (size_t i = 0; i < png_file->num_chunks; i++) {
        PNGChunk *chunk = &png_file->chunks[i];
        chunk->length = ((uint32_t *)result->ast->seq->elements[1]->seq->elements[i]->seq->elements[0]->uint32)[0];
        chunk->chunk_type = ((uint32_t *)result->ast->seq->elements[1]->seq->elements[i]->seq->elements[1]->uint32)[0];
        chunk->data = (uint8_t *)result->ast->seq->elements[1]->seq->elements[i]->seq->elements[2]->seq->elements;
        chunk->crc = ((uint32_t *)result->ast->seq->elements[1]->seq->elements[i]->seq->elements[3]->uint32)[0];
    }

    h_free_parse_result(result);
    return png_file;
}

// Function to free a PNG file
void free_png_file(PNGFile *png_file) {
    if (png_file) {
        if (png_file->chunks) {
            free(png_file->chunks);
        }
        free(png_file);
    }
}

int main() {
    // Example usage
    uint8_t png_data[] = {
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, // Signature
        0x00, 0x00, 0x00, 0x0D, // Length
        0x49, 0x48, 0x44, 0x52, // Chunk Type (IHDR)
        0x00, 0x00, 0x01, 0x00, // Data
        0x00, 0x00, 0x01, 0x00,
        0x08, 0x06, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, // CRC
    };

    PNGFile *png_file = parse_png_file(png_data, sizeof(png_data));
    if (png_file) {
        printf("PNG file parsed successfully\n");
        free_png_file(png_file);
    }

    return 0;
}