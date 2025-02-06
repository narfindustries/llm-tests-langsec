#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint8_t byte;
typedef uint16_t short_t;
typedef uint32_t long_t;
typedef struct { long_t num, den; } rational_t;

static hammer_parser_t read_bytes(size_t n) {
    return hammer_map(hammer_take(n), (hammer_map_f)memcpy);
}

static hammer_parser_t parse_tiff_header() {
    return hammer_and(
        hammer_choice(
            hammer_map(read_bytes(2), [](void* buf){ return *(short_t*)buf == 0x4949; }),
            hammer_map(read_bytes(2), [](void* buf){ return *(short_t*)buf == 0x4D4D; })
        ),
        hammer_map(read_bytes(2), [](void* buf){ return *(short_t*)buf == 0x002A; })
    );
}

static hammer_parser_t parse_ifd_entry() {
  return hammer_map(
      hammer_sequence4(
          read_bytes(2), // Tag
          read_bytes(2), // Type
          read_bytes(4), // Count
          read_bytes(4)  // Value or offset
      ),
      NULL //Replace with actual processing logic
  );
}

static hammer_parser_t parse_ifd() {
    return hammer_many(parse_ifd_entry());
}

static hammer_parser_t parse_tiff() {
    return hammer_sequence(
        parse_tiff_header(),
        hammer_map(read_bytes(4), [](void* buf){ return *(long_t*)buf; }), 
        parse_ifd()
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* fileContent = (char*)malloc(fileSize);
    fread(fileContent, 1, fileSize, fp);
    fclose(fp);

    hammer_result_t result = hammer_parse(parse_tiff(), fileContent, fileSize);

    if (result.success) {
        printf("TIFF file parsed successfully!\n");
    } else {
        fprintf(stderr, "Error parsing TIFF file: %s\n", result.error);
    }

    free(fileContent);
    return 0;
}

