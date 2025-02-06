#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_gzip_parser() {
    // Primitive parsers
    HParser *byte = h_uint8();
    HParser *u16 = h_uint16();
    HParser *u32 = h_uint32();

    // ID1 and ID2
    HParser *id1 = h_ch(0x1F);
    HParser *id2 = h_ch(0x8B);

    // CM
    HParser *cm = h_ch(0x08);

    // FLG
    HParser *flg = h_bits(8, false);

    // MTIME
    HParser *mtime = u32;

    // XFL
    HParser *xfl = byte;

    // OS
    HParser *os = byte;

    // Extra field (optional)
    HParser *xlen = u16;
    HParser *extra_field = h_sequence(xlen, h_repeat_n(byte, h_n(xlen)), NULL);
    HParser *extra_field_opt = h_optional(extra_field);

    // Original file name (optional)
    HParser *file_name = h_many1(byte);
    HParser *file_name_opt = h_optional(h_right(file_name, h_ch(0x00)));

    // File comment (optional)
    HParser *comment = h_many1(byte);
    HParser *comment_opt = h_optional(h_right(comment, h_ch(0x00)));

    // Header CRC16 (optional)
    HParser *header_crc16 = h_optional(u16);

    // CRC32
    HParser *crc32 = u32;

    // Compressed data (remaining bytes until CRC32)
    HParser *compressed_data = h_many(byte);

    // ISIZE
    HParser *isize = u32;

    // GZIP parser
    HParser *gzip_parser = h_sequence(
        id1, id2, cm, flg, mtime, xfl, os,
        extra_field_opt, file_name_opt, comment_opt, header_crc16,
        compressed_data, crc32, isize, NULL
    );

    return gzip_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
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

    HParser *gzip_parser = create_gzip_parser();
    HParseResult *result = h_parse(gzip_parser, data, file_size);

    if (result && result->ast) {
        printf("GZIP file parsed successfully.\n");
    } else {
        printf("Failed to parse GZIP file.\n");
    }

    h_parse_result_free(result);
    h_parser_unref(gzip_parser);
    free(data);

    return EXIT_SUCCESS;
}