#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flg;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    uint16_t xlen;
    uint8_t *extra_field;
    char *fname;
    char *fcomment;
    uint16_t hcrc;
    uint8_t *compressed_data;
    uint32_t crc32;
    uint32_t isize;
} GzipHeader;

HParser *gzip_parser() {
    HParser *id1 = h_int8();
    HParser *id2 = h_int8();
    HParser *cm = h_int8();
    HParser *flg = h_int8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_int8();
    HParser *os = h_int8();
    HParser *xlen = h_uint16();
    HParser *extra_field = h_repeat_n(h_uint8(), h_bits(h_uint16(), 16));
    HParser *fname = h_repeat_n(h_uint8(), h_not(h_uint8()));
    HParser *fcomment = h_repeat_n(h_uint8(), h_not(h_uint8()));
    HParser *hcrc = h_uint16();
    HParser *compressed_data = h_repeat_n(h_uint8(), h_uint32());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    return h_sequence(
        id1, id2, cm, flg, mtime, xfl, os, xlen, extra_field,
        h_optional(fname), h_optional(fcomment), h_optional(hcrc),
        compressed_data, crc32, isize, NULL
    );
}

void parse_gzip(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(1);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, data, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(data);
        exit(1);
    }

    free(data);
    h_parse_result_free(result);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip file>\n", argv[0]);
        return 1;
    }

    parse_gzip(argv[1]);
    return 0;
}