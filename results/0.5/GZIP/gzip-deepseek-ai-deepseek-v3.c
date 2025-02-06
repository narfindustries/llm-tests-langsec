#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *gzip_parser() {
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *cm = h_uint8();
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();

    HParser *extra_fields = h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL);
    HParser *filename = h_length_value(h_uint8(), h_uint8());
    HParser *comment = h_length_value(h_uint8(), h_uint8());
    HParser *header_crc16 = h_uint16();

    HParser *header = h_sequence(
        id1, id2, cm, flg, mtime, xfl, os,
        h_optional(h_sequence(h_peek(h_uint8()), extra_fields, NULL)),
        h_optional(h_sequence(h_peek(h_uint8()), filename, NULL)),
        h_optional(h_sequence(h_peek(h_uint8()), comment, NULL)),
        h_optional(h_sequence(h_peek(h_uint8()), header_crc16, NULL)),
        NULL
    );

    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    return h_sequence(header, compressed_data, crc32, isize, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}