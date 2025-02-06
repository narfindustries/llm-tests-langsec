#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t compression_method;
    uint8_t flags;
    uint32_t modification_time;
    uint8_t extra_flags;
    uint8_t os;
    uint16_t xlen;
    uint8_t *extra_field;
    char *filename;
    char *comment;
    uint16_t header_crc16;
    uint8_t *compressed_data;
    uint32_t crc32;
    uint32_t uncompressed_size;
} GzipHeader;

HParser *gzip_parser() {
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *compression_method = h_uint8();
    HParser *flags = h_uint8();
    HParser *modification_time = h_uint32();
    HParser *extra_flags = h_uint8();
    HParser *os = h_uint8();
    HParser *xlen = h_uint16();
    HParser *extra_field = h_many(h_uint8());
    HParser *filename = h_many(h_uint8());
    HParser *comment = h_many(h_uint8());
    HParser *header_crc16 = h_uint16();
    HParser *compressed_data = h_many(h_uint8());
    HParser *crc32 = h_uint32();
    HParser *uncompressed_size = h_uint32();

    return h_sequence(
        id1, id2, compression_method, flags, modification_time, extra_flags, os,
        h_optional(h_sequence(xlen, extra_field)),
        h_optional(h_sequence(filename)),
        h_optional(h_sequence(comment)),
        h_optional(h_sequence(header_crc16)),
        compressed_data,
        crc32,
        uncompressed_size,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
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

    HParser *parser = gzip_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(buffer);
        return 1;
    }

    GzipHeader *header = (GzipHeader *)result->ast;
    printf("GZIP Header:\n");
    printf("ID1: 0x%02X\n", header->id1);
    printf("ID2: 0x%02X\n", header->id2);
    printf("Compression Method: 0x%02X\n", header->compression_method);
    printf("Flags: 0x%02X\n", header->flags);
    printf("Modification Time: %u\n", header->modification_time);
    printf("Extra Flags: 0x%02X\n", header->extra_flags);
    printf("OS: 0x%02X\n", header->os);
    printf("XLEN: %u\n", header->xlen);
    printf("Filename: %s\n", header->filename);
    printf("Comment: %s\n", header->comment);
    printf("Header CRC-16: 0x%04X\n", header->header_crc16);
    printf("CRC-32: 0x%08X\n", header->crc32);
    printf("Uncompressed Size: %u\n", header->uncompressed_size);

    h_parse_result_free(result);
    free(buffer);
    return 0;
}