#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

HParser *gzip_parser() {
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *cm = h_uint8();
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();

    HParser *extra = h_optional(h_sequence(h_uint16(), h_length_value(h_uint16(), h_many(h_uint8())), NULL));
    HParser *fname = h_optional(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *fcomment = h_optional(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *crc16 = h_optional(h_uint16());

    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    HParser *gzip_header = h_sequence(id1, id2, cm, flg, mtime, xfl, os, extra, fname, fcomment, crc16, NULL);
    HParser *gzip_file = h_sequence(gzip_header, compressed_data, crc32, isize, NULL);

    return gzip_file;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *gzip = gzip_parser();
    HParseResult *result = h_parse(gzip, data, file_size);

    if (result) {
        printf("GZIP file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GZIP file\n");
    }

    free(data);
    return 0;
}