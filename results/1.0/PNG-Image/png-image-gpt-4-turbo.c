#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// Define the parsers for various parts of a PNG file
static HParser *uint8 = h_uint8();
static HParser *uint16_be = h_uint16_be();
static HParser *uint32_be = h_uint32_be();
static HParser *chunk_type = h_repeat_n(uint8, 4);

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDR_content;

// For parsing IHDR content
static HParsedToken *act_IHDR(const HParseResult *p, void *user_data) {
    const uint8_t *q = p->ast->seq->elements[0]->bytes->token;
    return H_MAKE_SEQ((H_MAKE_UINT(((uint32_t)q[0] << 24) | ((uint32_t)q[1] << 16) | ((uint32_t)q[2] << 8) | q[3]),
                       (H_MAKE_UINT(((uint32_t)q[4] << 24) | ((uint32_t)q[5] << 16) | ((uint32_t)q[6] << 8) | q[7]),
                        (H_MAKE_UINT(q[8]),
                         (H_MAKE_UINT(q[9]),
                          (H_MAKE_UINT(q[10]),
                           (H_MAKE_UINT(q[11]),
                            (H_MAKE_UINT(q[12])))))))));
}

static HParser *ihdr_chunk_data = h_action(h_sequence(uint32_be, uint32_be, uint8, uint8, uint8, uint8, uint8, NULL), act_IHDR, NULL);

// Generic chunk
typedef struct {
    uint32_t length;
    uint8_t *type;
    uint8_t *data;
    uint32_t crc;
} png_chunk;

// Action to convert parsed data into a more structured form
static HParsedToken *act_chunk(const HParseResult *p, void *user_data) {
    const HCountedArray *seq = p->ast->seq;
    uint32_t length = H_CAST_UINT(seq->elements[0]);
    const uint8_t *type = seq->elements[1]->bytes->token;
    const HParsedToken *data_token = seq->elements[2];
    uint32_t crc = H_CAST_UINT(seq->elements[3]);

    png_chunk *chunk = malloc(sizeof(png_chunk));
    chunk->length = length;
    chunk->type = malloc(4);
    memcpy(chunk->type, type, 4);
    chunk->data = malloc(length);
    memcpy(chunk->data, data_token->bytes->token, length);
    chunk->crc = crc;
    return H_MAKE_CUSTOM(chunk);
}

// Parser combinator for a PNG chunk
static HParser *png_chunk_parser = h_action(
    h_sequence(
        uint32_be,
        chunk_type,
        h_length_value(h_uint32(), h_bytes(1)),  // Using length-determined sequence for data
        uint32_be,
        NULL
    ), act_chunk, NULL
);

// Define the main PNG parser
static HParser *png_signature = h_chunk((const uint8_t *)"\x89PNG\r\n\x1a\n", 8);
static HParser *png_file = h_sequence(png_signature, h_many(png_chunk_parser), h_end_p(), NULL);

int main(int argc, char *argv[]) {
    size_t input_size;
    uint8_t *input_buffer;
    
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }
    
    fseek(file, 0, SEEK_END);
    input_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    input_buffer = malloc(input_size);
    if (!input_buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }
    
    if (fread(input_buffer, 1, input_size, file) != input_size) {
        perror("Failed to read file");
        free(input_buffer);
        fclose(file);
        return EXIT_FAILURE;
    }
    
    fclose(file);
    
    HParseResult *result = h_parse(png_file, input_buffer, input_size);
    if (result) {
        printf("Parsing completed successfully.\n");
    } else {
        printf("Parsing failed.\n");
    }
    
    free(input_buffer);
    return result ? EXIT_SUCCESS : EXIT_FAILURE;
}