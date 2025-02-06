#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *gzip_parser() {
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *cm = h_uint8();
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32_le();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();

    HParser *extra_len = h_if(h_bits_any(flg, 0x04), h_uint16_le(), h_nothing_p());
    HParser *extra = h_if(h_bits_any(flg, 0x04), h_length_value(extra_len, h_uint8()), h_nothing_p());

    HParser *fname = h_if(h_bits_any(flg, 0x08), h_while(h_not(h_uint8()), h_uint8()), h_nothing_p());
    HParser *fcomment = h_if(h_bits_any(flg, 0x10), h_while(h_not(h_uint8()), h_uint8()), h_nothing_p());
    HParser *fhcrc = h_if(h_bits_any(flg, 0x02), h_uint16_le(), h_nothing_p());

    HParser *compressed_data = h_until(h_uint8(), h_end_p());
    HParser *crc32 = h_uint32_le();
    HParser *isize = h_uint32_le();

    return h_sequence(id1, id2, cm, flg, mtime, xfl, os, extra, fname, fcomment, fhcrc, compressed_data, crc32, isize, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("GZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GZIP file.\n");
    }

    free(data);
    h_parser_free(parser);
    return EXIT_SUCCESS;
}