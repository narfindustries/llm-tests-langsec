#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDR;

typedef struct {
    uint32_t length;
    char type[4];
    union {
        IHDR ihdr;
        // Add other chunk types here as needed...
    } data;
    uint32_t crc;
} Chunk;


hammer_parser_t* uint32_parser() {
    return hammer_uint32_le();
}

hammer_parser_t* uint8_parser() {
    return hammer_uint8();
}

hammer_parser_t* string4_parser() {
    return hammer_string(4);
}

hammer_parser_t* crc32_parser() {
    return hammer_uint32_le();
}

hammer_parser_t* ihdr_data_parser() {
    return hammer_map(
        hammer_seq(
            uint32_parser(),
            uint32_parser(),
            uint8_parser(),
            uint8_parser(),
            uint8_parser(),
            uint8_parser(),
            uint8_parser(),
            NULL
        ),
        (hammer_map_func_t) (void*) &IHDR
    );
}


hammer_parser_t* png_signature_parser() {
    return hammer_string_literal("\x89PNG\r\n\x1a\n");
}


hammer_parser_t* chunk_parser() {
  return hammer_map(
    hammer_seq(
        uint32_parser(),
        string4_parser(),
        hammer_choice(
            ihdr_data_parser(),
            hammer_many(uint8_parser()), // idat, etc.
            hammer_empty(), // iend
            hammer_many(uint8_parser()), // plte, etc.
            hammer_many(uint8_parser()), // trns, etc.
            NULL
        ),
        crc32_parser(),
        NULL
    ),
    (hammer_map_func_t)(void*)&Chunk
  );
}

hammer_parser_t* png_parser() {
    return hammer_seq(
        png_signature_parser(),
        hammer_many(chunk_parser()),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buffer = (char*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* parser = png_parser();
    hammer_result_t result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        printf("PNG file parsed successfully!\n");
        // Access parsed data through result.value (a list of Chunk structs)
    } else {
        fprintf(stderr, "Error parsing PNG file: %s\n", result.error);
    }

    free(buffer);
    hammer_free(parser);
    return 0;
}
