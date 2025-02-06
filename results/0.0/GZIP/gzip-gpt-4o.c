#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *gzip_parser() {
    HParser *id1 = h_ch(0x1F);
    HParser *id2 = h_ch(0x8B);
    HParser *cm = h_ch(0x08);
    HParser *flg = h_bits(8, false);
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();

    HParser *extra_len = h_uint16();
    HParser *extra_field = h_bind_uint16(extra_len, h_repeat_n(h_uint8()));

    HParser *original_file_name = h_many1(h_not(h_ch(0x00)));
    HParser *file_comment = h_many1(h_not(h_ch(0x00)));
    HParser *header_crc16 = h_uint16();

    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    HParser *gzip_header = h_sequence(
        id1, id2, cm, flg, mtime, xfl, os,
        h_optional(h_sequence(extra_len, extra_field)),
        h_optional(original_file_name),
        h_optional(file_comment),
        h_optional(header_crc16),
        NULL
    );

    HParser *gzip_parser = h_sequence(
        gzip_header,
        compressed_data,
        crc32,
        isize,
        NULL
    );

    return gzip_parser;
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

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("GZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GZIP file.\n");
    }

    free(buffer);
    h_parser_free(parser);

    return EXIT_SUCCESS;
}