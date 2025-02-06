#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint16_t TIFF_WORD;
typedef uint32_t TIFF_DWORD;

static TIFF_WORD read_word(hammer_parser_t *parser) {
    TIFF_WORD word;
    if (!hammer_parse_uint16(parser, &word)) return 0;
    return word;
}

static TIFF_DWORD read_dword(hammer_parser_t *parser) {
    TIFF_DWORD dword;
    if (!hammer_parse_uint32(parser, &dword)) return 0;
    return dword;
}

static hammer_result_t read_bytes(hammer_parser_t *parser, uint8_t *buffer, size_t len) {
    return hammer_parse_bytes(parser, buffer, len);
}

hammer_parser_t *tiff_ifd_entry(void) {
    return hammer_seq(
        hammer_map(hammer_parse_uint16, NULL),
        hammer_map(hammer_parse_uint16, NULL),
        hammer_map(hammer_parse_uint32, NULL),
        hammer_choice(
            hammer_map(hammer_parse_bytes, NULL),
            hammer_map(hammer_parse_uint32, NULL)
        ),
        NULL
    );
}

hammer_parser_t *tiff_ifd(void) {
    return hammer_seq(
        hammer_map(hammer_parse_uint16, NULL),
        hammer_many(tiff_ifd_entry()),
        NULL
    );
}

hammer_parser_t *tiff_parser(void) {
    return hammer_seq(
        hammer_string("II"),
        hammer_map(hammer_parse_uint16, NULL),
        hammer_map(hammer_parse_uint32, NULL),
        tiff_ifd(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t *parser = tiff_parser();
    hammer_parser_t *p = hammer_from_memory(buffer, fsize);
    hammer_result_t result = hammer_parse(p, parser);

    free(buffer);
    hammer_free(p);
    hammer_free(parser);

    if (result.success) {
        printf("TIFF file parsed successfully!\n");
    } else {
        fprintf(stderr, "Error parsing TIFF file: %s\n", result.error);
    }

    return 0;
}

