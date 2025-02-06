#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define LOCAL_FILE_HEADER_SIG 0x04034B50
#define CENTRAL_DIR_FILE_HEADER_SIG 0x02014B50
#define END_OF_CENTRAL_DIR_SIG 0x06054B50
#define ZIP64_END_OF_CENTRAL_DIR_SIG 0x06064B50

typedef enum {
    NO_COMPRESSION = 0,
    SHRUNK = 1,
    REDUCED1 = 2,
    REDUCED2 = 3,
    REDUCED3 = 4,
    REDUCED4 = 5,
    IMPLODED = 6,
    DEFLATED = 8,
    ENHANCED_DEFLATED = 9,
    PKWARE_DCL_IMPLODED = 10,
    BZIP2 = 12,
    LZMA = 14
} CompressionMethod;

HParser* parse_local_file_header() {
    return h_sequence(
        h_token((const uint8_t*)&(uint32_t){LOCAL_FILE_HEADER_SIG}, 4),
        h_uint16(),  // version needed
        h_uint16(),  // bit flag
        h_uint16(),  // compression method
        h_uint16(),  // last mod time
        h_uint16(),  // last mod date
        h_uint32(),  // crc32
        h_uint32(),  // compressed size
        h_uint32(),  // uncompressed size
        h_uint16(),  // filename length
        h_uint16(),  // extra field length
        h_repeat_n(h_ch_range('0', '9'), 1),  // dynamic filename
        h_many(h_uint8()),  // extra field
        NULL
    );
}

HParser* parse_central_directory_header() {
    return h_sequence(
        h_token((const uint8_t*)&(uint32_t){CENTRAL_DIR_FILE_HEADER_SIG}, 4),
        h_uint16(),  // version made by
        h_uint16(),  // version needed
        h_uint16(),  // bit flag
        h_uint16(),  // compression method
        h_uint16(),  // last mod time
        h_uint16(),  // last mod date
        h_uint32(),  // crc32
        h_uint32(),  // compressed size
        h_uint32(),  // uncompressed size
        h_uint16(),  // filename length
        h_uint16(),  // extra field length
        h_uint16(),  // file comment length
        h_uint16(),  // disk number start
        h_uint16(),  // internal file attributes
        h_uint32(),  // external file attributes
        h_uint32(),  // local header offset
        h_repeat_n(h_ch_range('0', '9'), 1),  // filename
        h_many(h_uint8()),  // extra field
        h_repeat_n(h_ch_range(' ', '~'), 1),  // file comment
        NULL
    );
}

HParser* parse_end_of_central_directory() {
    return h_sequence(
        h_token((const uint8_t*)&(uint32_t){END_OF_CENTRAL_DIR_SIG}, 4),
        h_uint16(),  // number of this disk
        h_uint16(),  // disk with central directory
        h_uint16(),  // total entries on this disk
        h_uint16(),  // total entries in directory
        h_uint32(),  // size of central directory
        h_uint32(),  // offset of central directory
        h_uint16(),  // comment length
        h_repeat_n(h_ch_range(' ', '~'), 1),  // comment
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* zip_parser = h_choice(
        parse_local_file_header(),
        parse_central_directory_header(),
        parse_end_of_central_directory(),
        NULL
    );

    HParseResult* result = h_parse(zip_parser, buffer, file_size);
    if (result) {
        printf("Successfully parsed ZIP file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}