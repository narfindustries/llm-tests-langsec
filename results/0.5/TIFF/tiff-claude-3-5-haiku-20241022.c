#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef enum {
    TIFF_LITTLE_ENDIAN = 0x4949,
    TIFF_BIG_ENDIAN = 0x4D4D
} TiffByteOrder;

typedef enum {
    TIFF_MAGIC_NUMBER = 0x002A
} TiffConstants;

typedef enum {
    COMPRESSION_NONE = 1,
    COMPRESSION_CCITT3 = 2,
    COMPRESSION_CCITT4 = 3,
    COMPRESSION_LZW = 4,
    COMPRESSION_JPEG = 5,
    COMPRESSION_PACKBITS = 32773
} TiffCompressionType;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_or_offset;
} TiffTag;

HParser* parse_byte_order() {
    return h_choice(
        h_token((const char*)&(uint16_t){TIFF_LITTLE_ENDIAN}, 2),
        h_token((const char*)&(uint16_t){TIFF_BIG_ENDIAN}, 2),
        NULL
    );
}

HParser* parse_magic_number() {
    return h_token((const char*)&(uint16_t){TIFF_MAGIC_NUMBER}, 2);
}

HParser* parse_ifd_tag() {
    return h_sequence(
        h_uint16(),   // tag
        h_uint16(),   // type
        h_uint32(),   // count
        h_uint32(),   // value or offset
        NULL
    );
}

HParser* create_tiff_parser() {
    return h_sequence(
        parse_byte_order(),
        parse_magic_number(),
        h_uint32(),  // first IFD offset
        h_uint16(),  // number of directory entries
        h_many(parse_ifd_tag()),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
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

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* tiff_parser = create_tiff_parser();
    HParseResult* result = h_parse(tiff_parser, buffer, file_size);

    if (result && result->ast) {
        printf("TIFF file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("TIFF parsing failed\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}