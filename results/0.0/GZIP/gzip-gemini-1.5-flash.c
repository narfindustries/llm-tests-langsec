#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <zlib.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flg;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    uint16_t extra_len;
    uint8_t *extra;
    char *filename;
    char *comment;
    uint16_t header_crc;
    uint8_t *compressed_data;
    size_t compressed_data_len;
    uint32_t crc32;
    uint32_t isize;

} gzip_data_t;


// Helper function to read a specific number of bytes from a file
size_t read_bytes(FILE *fp, void *buffer, size_t count) {
    return fread(buffer, 1, count, fp);
}


// Hammer parser combinator definitions for GZIP fields
hammer_parser_t *gzip_id1() {
    return hammer_uint8(0x1f);
}

hammer_parser_t *gzip_id2() {
    return hammer_uint8(0x8b);
}

hammer_parser_t *gzip_cm() {
    return hammer_uint8(0x08);
}

hammer_parser_t *gzip_flg() {
    return hammer_uint8(0); 
}

hammer_parser_t *gzip_mtime() {
    return hammer_uint32_be();
}

hammer_parser_t *gzip_xfl() {
    return hammer_uint8(0); 
}

hammer_parser_t *gzip_os() {
    return hammer_uint8(0); 
}

hammer_parser_t *gzip_extra_field(size_t len) {
    return hammer_bytes(len);
}

hammer_parser_t *gzip_filename() {
    return hammer_string_until('\0');
}

hammer_parser_t *gzip_comment() {
    return hammer_string_until('\0');
}

hammer_parser_t *gzip_header_crc() {
    return hammer_uint16_be();
}

hammer_parser_t *gzip_crc32() {
    return hammer_uint32_be();
}

hammer_parser_t *gzip_isize() {
    return hammer_uint32_be();
}


hammer_parser_t *gzip_header() {
    return hammer_seq(
        gzip_id1(),
        gzip_id2(),
        gzip_cm(),
        gzip_flg(),
        gzip_mtime(),
        gzip_xfl(),
        gzip_os(),
        NULL
    );
}

hammer_parser_t *parse_gzip_optional_fields(hammer_parser_t *flg_parser) {
  return hammer_seq(
    hammer_map(
      flg_parser,
      (hammer_map_func_t) [](uint8_t flg) {
        return (flg & 0x04) ? hammer_uint16_be() : hammer_nothing();
      }
    ),
    hammer_map(
      flg_parser,
      (hammer_map_func_t) [](uint8_t flg) {
        return (flg & 0x08) ? gzip_filename() : hammer_nothing();
      }
    ),
    hammer_map(
      flg_parser,
      (hammer_map_func_t) [](uint8_t flg) {
        return (flg & 0x10) ? gzip_comment() : hammer_nothing();
      }
    ),
    NULL
  );
}

hammer_parser_t *parse_gzip_optional_fields_with_header_crc(hammer_parser_t *flg_parser) {
    return hammer_seq(
        parse_gzip_optional_fields(flg_parser),
        hammer_map(
            flg_parser,
            (hammer_map_func_t) [](uint8_t flg){
                return (flg & 0x02) ? gzip_header_crc() : hammer_nothing();
            }
        ),
        NULL
    );
}

hammer_parser_t *gzip_parser() {
    return hammer_seq(
        hammer_bind(
            gzip_header(),
            (hammer_bind_func_t) [](hammer_result_t *header_result){
                uint8_t flg = *(uint8_t *)hammer_result_get_field(header_result, 3);
                return hammer_seq(
                    parse_gzip_optional_fields_with_header_crc(hammer_uint8(flg)),
                    hammer_many(hammer_any()), 
                    gzip_crc32(),
                    gzip_isize(),
                    NULL
                );
            }
        ),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, fsize, fp) != fsize) {
        perror("Error reading file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    hammer_result_t *result = hammer_parse(gzip_parser(), buffer, fsize);

    if (result->status == HAMMER_STATUS_SUCCESS) {
        printf("GZIP file parsed successfully!\n");
    } else {
        fprintf(stderr, "GZIP parsing failed: %s\n", hammer_result_error(result));
    }

    hammer_result_free(result);
    free(buffer);

    return 0;
}

