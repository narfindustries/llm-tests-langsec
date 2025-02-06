#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

#define GZIP_ID1 0x1f
#define GZIP_ID2 0x8b
#define CM_DEFLATE 0x08

#define FTEXT    0x01
#define FHCRC    0x02
#define FEXTRA   0x04
#define FNAME    0x08
#define FCOMMENT 0x10

HParser *gzip_parser();

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    rewind(fp);

    uint8_t *data = malloc(size);
    if (data == NULL) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(data, 1, size, fp);
    fclose(fp);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, data, size);

    if (result) {
        printf("Parse successful\n");
    } else {
        fprintf(stderr, "Parse failed\n");
    }

    h_parse_result_free(result);
    h_parser_cleanup(parser);  // ensure proper cleanup function is called depending on Hammer version/ update
    free(data);

    return 0;
}

HParser *gzip_parser() {
    HParser *id1 = h_bits(8, false);
    HParser *id2 = h_bits(8, false);
    HParser *cm = h_bits(8, false);
    HParser *flg = h_bits(8, false);
    HParser *mtime = h_bits(32, false);
    HParser *xfl = h_bits(8, false);
    HParser *os = h_bits(8, false);
    
    HParser *xlen = h_bits(16, false);
    HParser *extra = h_length_value(h_uint16(), h_bytes(1));
    HParser *fname = h_null_terminated();
    HParser *fcomment = h_null_terminated();
    HParser *fhcrc = h_bits(16, false);

    HParser *header = h_sequence(id1, id2, cm, flg, mtime, xfl, os, NULL);

    HParser *fextra = h_optional(h_sequence(flg, h_attr_bool(flg, FEXTRA), xlen, extra, NULL));
    HParser *ffname = h_optional(h_sequence(flg, h_attr_bool(flg, FNAME), fname, NULL));
    HParser *fcomment_field = h_optional(h_sequence(flg, h_attr_bool(flg, FCOMMENT), fcomment, NULL));
    HParser *fhcrc_field = h_optional(fhcrc);

    HParser *crc32 = h_bits(32, false);
    HParser *isize = h_bits(32, false);
    HParser *footer = h_sequence(crc32, isize, NULL);

    return h_sequence(header, fextra, ffname, fcomment_field, fhcrc_field, h_greedy1(h_uint8()), footer, NULL);
}