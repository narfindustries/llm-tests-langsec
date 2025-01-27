#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// GZIP header structure
typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flg;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    uint8_t *extra;
    uint16_t extra_len;
    char *fname;
    char *fcomment;
    uint16_t crc16;
} gzip_header_t;

// GZIP footer structure
typedef struct {
    uint32_t crc32;
    uint32_t isize;
} gzip_footer_t;

// Parser for GZIP header
HParser *gzip_header_parser() {
    return h_sequence(
        h_uint8(), // ID1
        h_uint8(), // ID2
        h_uint8(), // CM
        h_uint8(), // FLG
        h_uint32(), // MTIME
        h_uint8(), // XFL
        h_uint8(), // OS
        h_optional(h_sequence( // Extra field (optional)
            h_uint16(), // XLEN
            h_length_value(h_uint16(), h_uint8()) // Extra data
        )),
        h_optional(h_length_value(h_uint8(), h_uint8())), // FNAME (optional)
        h_optional(h_length_value(h_uint8(), h_uint8())), // FCOMMENT (optional)
        h_optional(h_uint16()) // CRC16 (optional)
    );
}

// Parser for GZIP footer
HParser *gzip_footer_parser() {
    return h_sequence(
        h_uint32(), // CRC32
        h_uint32() // ISIZE
    );
}

// Main function
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(gzip_header_parser(), buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse GZIP header\n");
        free(buffer);
        return 1;
    }

    gzip_header_t *header = (gzip_header_t *)result->ast;
    printf("GZIP Header:\n");
    printf("ID1: 0x%02x, ID2: 0x%02x\n", header->id1, header->id2);
    printf("CM: %u, FLG: 0x%02x\n", header->cm, header->flg);
    printf("MTIME: %u, XFL: %u, OS: %u\n", header->mtime, header->xfl, header->os);

    if (header->extra) {
        printf("Extra field length: %u\n", header->extra_len);
    }

    if (header->fname) {
        printf("Filename: %s\n", header->fname);
    }

    if (header->fcomment) {
        printf("Comment: %s\n", header->fcomment);
    }

    if (header->crc16) {
        printf("CRC16: 0x%04x\n", header->crc16);
    }

    result = h_parse(gzip_footer_parser(), buffer + file_size - 8, 8);
    if (!result) {
        fprintf(stderr, "Failed to parse GZIP footer\n");
        free(buffer);
        return 1;
    }

    gzip_footer_t *footer = (gzip_footer_t *)result->ast;
    printf("GZIP Footer:\n");
    printf("CRC32: 0x%08x\n", footer->crc32);
    printf("ISIZE: %u\n", footer->isize);

    free(buffer);
    return 0;
}