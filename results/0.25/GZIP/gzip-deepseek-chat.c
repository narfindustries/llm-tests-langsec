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
        h_uint16()  // CRC16
    );
}

// Parser for GZIP footer
HParser* gzip_footer_parser() {
    return h_sequence(
        h_uint32(), // CRC32
        h_uint32()  // ISIZE
    );
}

// Parser for GZIP file
HParser* gzip_parser() {
    return h_sequence(
        gzip_header_parser(),
        h_many(h_uint8()), // Compressed data
        gzip_footer_parser()
    );
}

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

    HParser* parser = gzip_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(buffer);
        return 1;
    }

    printf("GZIP file parsed successfully\n");

    h_parse_result_free(result);
    free(buffer);

    return 0;
}