#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define CHUNK_SIZE 4

typedef enum {
    COLOR_TYPE_GRAYSCALE = 0,
    COLOR_TYPE_RGB = 2,
    COLOR_TYPE_PLTE = 3,
    COLOR_TYPE_GRAYSCALE_ALPHA = 4,
    COLOR_TYPE_RGBA = 6
} color_type_t;

typedef enum {
    COMPRESSION_METHOD_DEFLATE = 0
} compression_method_t;

typedef enum {
    FILTER_METHOD_ADAPTIVE = 0
} filter_method_t;

typedef enum {
    INTERLACE_METHOD_NONE = 0,
    INTERLACE_METHOD_ADAM7 = 1
} interlace_method_t;

typedef struct {
    uint32_t length;
    char chunk_type[5];
    uint32_t crc;
} chunk_header_t;

typedef struct {
    chunk_header_t header;
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} ihdr_chunk_t;

typedef struct {
    chunk_header_t header;
    uint8_t palette[256 * 3];
} plte_chunk_t;

typedef struct {
    chunk_header_t header;
    uint8_t data[1024];
} idat_chunk_t;

typedef struct {
    chunk_header_t header;
} iend_chunk_t;

typedef struct {
    chunk_header_t header;
    uint8_t key[256];
    uint8_t text[256];
} text_chunk_t;

#define CHUNK_HEADER_SIZE sizeof(chunk_header_t)
#define IHDR_CHUNK_SIZE sizeof(ihdr_chunk_t)
#define PLTE_CHUNK_SIZE sizeof(plte_chunk_t)
#define IDAT_CHUNK_SIZE sizeof(idat_chunk_t)
#define IEND_CHUNK_SIZE sizeof(iend_chunk_t)
#define TEXT_CHUNK_SIZE sizeof(text_chunk_t)

typedef struct {
    uint8_t* data;
    size_t size;
} hammer_parser_t;

typedef struct {
    int success;
    char* error;
} hammer_result_t;

hammer_parser_t* chunk_header_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + CHUNK_HEADER_SIZE;
    new_parser->size = parser->size - CHUNK_HEADER_SIZE;
    return new_parser;
}

hammer_parser_t* ihdr_chunk_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + IHDR_CHUNK_SIZE;
    new_parser->size = parser->size - IHDR_CHUNK_SIZE;
    return new_parser;
}

hammer_parser_t* plte_chunk_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + PLTE_CHUNK_SIZE;
    new_parser->size = parser->size - PLTE_CHUNK_SIZE;
    return new_parser;
}

hammer_parser_t* idat_chunk_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + IDAT_CHUNK_SIZE;
    new_parser->size = parser->size - IDAT_CHUNK_SIZE;
    return new_parser;
}

hammer_parser_t* iend_chunk_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + IEND_CHUNK_SIZE;
    new_parser->size = parser->size - IEND_CHUNK_SIZE;
    return new_parser;
}

hammer_parser_t* text_chunk_parser(hammer_parser_t* parser) {
    hammer_parser_t* new_parser = malloc(sizeof(hammer_parser_t));
    new_parser->data = parser->data + TEXT_CHUNK_SIZE;
    new_parser->size = parser->size - TEXT_CHUNK_SIZE;
    return new_parser;
}

hammer_parser_t* png_parser(hammer_parser_t* parser) {
    uint8_t magic_number[] = {137, 80, 78, 71, 13, 10, 26, 10};
    if (memcmp(parser->data, magic_number, 8) != 0) {
        printf("Invalid PNG file\n");
        return NULL;
    }
    parser->data += 8;
    parser->size -= 8;
    return parser;
}

hammer_result_t* hammer_parse(hammer_parser_t* parser, uint8_t* data, size_t size) {
    hammer_result_t* result = malloc(sizeof(hammer_result_t));
    parser->data = data;
    parser->size = size;
    parser = png_parser(parser);
    if (parser == NULL) {
        result->success = 0;
        result->error = "Invalid PNG file";
        return result;
    }
    result->success = 1;
    result->error = NULL;
    return result;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    hammer_result_t* result = hammer_parse(parser, data, file_size);

    if (result->success) {
        printf("PNG file parsed successfully\n");
    } else {
        printf("Error parsing PNG file: %s\n", result->error);
    }

    free(data);
    free(parser);
    free(result);
    return 0;
}