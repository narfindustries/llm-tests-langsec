#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t signature[8];
    HParseResult* chunks;
} PNG;

typedef struct {
    uint32_t length;
    char type[4];
    uint8_t* data;
    uint32_t crc;
} Chunk;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDR;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* file_data = (uint8_t*)malloc(file_size);
    if (!file_data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(file_data, 1, file_size, file);
    fclose(file);

    HParser* png_signature_parser = h_sequence(h_bytes(1, 8), h_end_p());
    HParser* chunk_length_parser = h_uint32();
    HParser* chunk_type_parser = h_sequence(h_bytes(1, 4), h_end_p());
    HParser* chunk_crc_parser = h_uint32();

    HParser* chunk_parser = h_sequence(
        chunk_length_parser,
        chunk_type_parser,
        h_bytes(1, h_act_uint32(chunk_length_parser)),
        chunk_crc_parser,
        h_end_p()
    );

    HParser* chunks_parser = h_many1(chunk_parser);
    HParser* png_parser = h_sequence(png_signature_parser, chunks_parser, h_end_p());

    HParseResult* result = h_parse(png_parser, file_data, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(file_data);
        return 1;
    }

    PNG* png = (PNG*)result->ast;
    printf("PNG signature: %.8s\n", png->signature);

    h_parse_result_free(result);
    free(file_data);
    return 0;
}