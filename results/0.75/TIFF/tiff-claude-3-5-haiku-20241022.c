#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef enum {
    COMPRESSION_NONE = 1,
    COMPRESSION_CCITT_1D = 2,
    COMPRESSION_CCITT_2D = 3,
    COMPRESSION_CCITT_4 = 4,
    COMPRESSION_LZW = 5,
    COMPRESSION_JPEG = 6,
    COMPRESSION_PACKBITS = 32773
} CompressionType;

typedef enum {
    PHOTOMETRIC_WHITE_IS_ZERO = 0,
    PHOTOMETRIC_BLACK_IS_ZERO = 1,
    PHOTOMETRIC_RGB = 2,
    PHOTOMETRIC_PALETTE = 3,
    PHOTOMETRIC_TRANSPARENCY_MASK = 4,
    PHOTOMETRIC_CMYK = 5,
    PHOTOMETRIC_YCBCR = 6
} PhotometricInterpretation;

typedef enum {
    ORIENTATION_TOP_LEFT = 1,
    ORIENTATION_TOP_RIGHT = 2,
    ORIENTATION_BOTTOM_RIGHT = 3,
    ORIENTATION_BOTTOM_LEFT = 4,
    ORIENTATION_LEFT_TOP = 5,
    ORIENTATION_RIGHT_TOP = 6,
    ORIENTATION_RIGHT_BOTTOM = 7,
    ORIENTATION_LEFT_BOTTOM = 8
} Orientation;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_or_offset;
} TiffDirectoryEntry;

typedef struct {
    uint16_t byte_order;
    uint16_t magic_number;
    uint32_t ifd_offset;
    uint16_t num_directory_entries;
    TiffDirectoryEntry* entries;
} TiffHeader;

HParser* byte_order_parser() {
    uint8_t little_endian[] = {0x49, 0x49};
    uint8_t big_endian[] = {0x4D, 0x4D};
    return h_choice(
        h_token(little_endian, sizeof(little_endian)),
        h_token(big_endian, sizeof(big_endian)),
        NULL
    );
}

HParser* magic_number_parser() {
    uint8_t magic[] = {0x00, 0x2A};
    return h_token(magic, sizeof(magic));
}

HParser* tiff_header_parser() {
    return h_sequence(
        byte_order_parser(),
        magic_number_parser(),
        h_uint32(),
        h_uint16(),
        NULL
    );
}

HParser* directory_entry_parser() {
    return h_sequence(
        h_uint16(),   // tag
        h_uint16(),   // type
        h_uint32(),   // count
        h_uint32(),   // value or offset
        NULL
    );
}

int parse_tiff_file(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return -1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return -1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return -1;
    }

    HParser* header_parser = tiff_header_parser();
    HParseResult* header_result = h_parse(header_parser, buffer, file_size);

    if (!header_result) {
        fprintf(stderr, "TIFF header parsing failed\n");
        free(buffer);
        return -1;
    }

    HParser* directory_parser = h_repeat_n(directory_entry_parser(), 1);
    HParseResult* directory_result = h_parse(directory_parser, buffer, file_size);

    if (!directory_result) {
        fprintf(stderr, "TIFF directory parsing failed\n");
        h_parse_result_free(header_result);
        free(buffer);
        return -1;
    }

    h_parse_result_free(header_result);
    h_parse_result_free(directory_result);
    free(buffer);
    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    return parse_tiff_file(argv[1]);
}