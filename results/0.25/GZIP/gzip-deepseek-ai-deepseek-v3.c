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
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *cm = h_uint8();
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();
    HParser *xlen = h_uint16();
    HParser *extra_field = h_many(h_uint8());
    HParser *fname = h_many(h_uint8());
    HParser *fcomment = h_many(h_uint8());
    HParser *hcrc = h_uint16();
    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    return h_sequence(
        id1, id2, cm, flg, mtime, xfl, os, xlen, extra_field, fname, fcomment, hcrc, compressed_data, crc32, isize, NULL
    );
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

    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(buffer);
        return 1;
    }

    GzipHeader *header = (GzipHeader *)result->ast;
    printf("ID1: 0x%02X\n", header->id1);
    printf("ID2: 0x%02X\n", header->id2);
    printf("CM: 0x%02X\n", header->cm);
    printf("FLG: 0x%02X\n", header->flg);
    printf("MTIME: %u\n", header->mtime);
    printf("XFL: 0x%02X\n", header->xfl);
    printf("OS: 0x%02X\n", header->os);
    printf("XLEN: %u\n", header->xlen);
    printf("Extra Field: %p\n", header->extra_field);
    printf("FNAME: %s\n", header->fname);
    printf("FCOMMENT: %s\n", header->fcomment);
    printf("HCRC: 0x%04X\n", header->hcrc);
    printf("Compressed Data: %p\n", header->compressed_data);
    printf("CRC32: 0x%08X\n", header->crc32);
    printf("ISIZE: %u\n", header->isize);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}