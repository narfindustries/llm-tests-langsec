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
    uint8_t* extra;
    uint8_t* name;
    uint8_t* comment;
    uint16_t crc16;
} gzip_header_t;

// GZIP footer structure
typedef struct {
    uint32_t crc32;
    uint32_t isize;
} gzip_footer_t;

// Parser for GZIP header
HParser* gzip_header_parser() {
    return h_sequence(
        h_uint8(),  // ID1
        h_uint8(),  // ID2
        h_uint8(),  // CM
        h_uint8(),  // FLG
        h_uint32(), // MTIME
        h_uint8(),  // XFL
        h_uint8(),  // OS
        h_optional(h_length_value(h_uint16(), h_uint8())), // EXTRA
        h_optional(h_length_value(h_uint8(), h_uint8())),  // NAME
        h_optional(h_length_value(h_uint8(), h_uint8())),  // COMMENT
        h_optional(h_uint16()), // CRC16
        NULL
    );
}

// Parser for GZIP footer
HParser* gzip_footer_parser() {
    return h_sequence(
        h_uint32(), // CRC32
        h_uint32(), // ISIZE
        NULL
    );
}

// Main function
int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* header_parser = gzip_header_parser();
    HParser* footer_parser = gzip_footer_parser();

    HParseResult* header_result = h_parse(header_parser, buffer, file_size);
    if (!header_result) {
        fprintf(stderr, "Failed to parse GZIP header\n");
        free(buffer);
        return 1;
    }

    HParseResult* footer_result = h_parse(footer_parser, buffer + file_size - 8, 8);
    if (!footer_result) {
        fprintf(stderr, "Failed to parse GZIP footer\n");
        h_parse_result_free(header_result);
        free(buffer);
        return 1;
    }

    gzip_header_t* header = (gzip_header_t*)header_result->ast;
    gzip_footer_t* footer = (gzip_footer_t*)footer_result->ast;

    printf("GZIP Header:\n");
    printf("  ID1: 0x%02x\n", header->id1);
    printf("  ID2: 0x%02x\n", header->id2);
    printf("  CM: 0x%02x\n", header->cm);
    printf("  FLG: 0x%02x\n", header->flg);
    printf("  MTIME: 0x%08x\n", header->mtime);
    printf("  XFL: 0x%02x\n", header->xfl);
    printf("  OS: 0x%02x\n", header->os);
    printf("  CRC16: 0x%04x\n", header->crc16);

    printf("GZIP Footer:\n");
    printf("  CRC32: 0x%08x\n", footer->crc32);
    printf("  ISIZE: 0x%08x\n", footer->isize);

    h_parse_result_free(header_result);
    h_parse_result_free(footer_result);
    free(buffer);

    return 0;
}