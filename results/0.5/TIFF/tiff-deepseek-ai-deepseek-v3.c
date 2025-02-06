#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define TIFF data types
typedef enum {
    TIFF_TYPE_BYTE = 1,
    TIFF_TYPE_ASCII = 2,
    TIFF_TYPE_SHORT = 3,
    TIFF_TYPE_LONG = 4,
    TIFF_TYPE_RATIONAL = 5,
    TIFF_TYPE_SBYTE = 6,
    TIFF_TYPE_UNDEFINED = 7,
    TIFF_TYPE_SSHORT = 8,
    TIFF_TYPE_SLONG = 9,
    TIFF_TYPE_SRATIONAL = 10,
    TIFF_TYPE_FLOAT = 11,
    TIFF_TYPE_DOUBLE = 12
} TiffDataType;

// Define TIFF tag structure
typedef struct {
    uint16_t tag;
    TiffDataType type;
    uint32_t count;
    uint32_t value_or_offset;
} TiffTag;

// Define TIFF IFD structure
typedef struct {
    uint16_t num_tags;
    TiffTag *tags;
    uint32_t next_ifd_offset;
} TiffIFD;

// Define TIFF header structure
typedef struct {
    uint16_t byte_order;
    uint16_t magic;
    uint32_t first_ifd_offset;
} TiffHeader;

// Parser for TIFF header
HParser *tiff_header_parser() {
    return h_sequence(
        h_uint16(), // Byte order
        h_uint16(), // Magic number
        h_uint32(), // First IFD offset
        NULL
    );
}

// Parser for TIFF tag
HParser *tiff_tag_parser() {
    return h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value/Offset
        NULL
    );
}

// Parser for TIFF IFD
HParser *tiff_ifd_parser() {
    return h_sequence(
        h_uint16(), // Number of tags
        h_many(tiff_tag_parser()), // Tags
        h_uint32(), // Next IFD offset
        NULL
    );
}

// Main function
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
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *header_parser = tiff_header_parser();
    HParseResult *header_result = h_parse(header_parser, buffer, file_size);
    if (!header_result) {
        fprintf(stderr, "Failed to parse TIFF header\n");
        free(buffer);
        return 1;
    }

    TiffHeader *header = (TiffHeader *)header_result->ast;
    printf("Byte Order: %04x\n", header->byte_order);
    printf("Magic: %04x\n", header->magic);
    printf("First IFD Offset: %08x\n", header->first_ifd_offset);

    HParser *ifd_parser = tiff_ifd_parser();
    HParseResult *ifd_result = h_parse(ifd_parser, buffer + header->first_ifd_offset, file_size - header->first_ifd_offset);
    if (!ifd_result) {
        fprintf(stderr, "Failed to parse TIFF IFD\n");
        free(buffer);
        return 1;
    }

    TiffIFD *ifd = (TiffIFD *)ifd_result->ast;
    printf("Number of Tags: %d\n", ifd->num_tags);
    for (int i = 0; i < ifd->num_tags; i++) {
        TiffTag *tag = &ifd->tags[i];
        printf("Tag: %04x, Type: %d, Count: %d, Value/Offset: %08x\n", tag->tag, tag->type, tag->count, tag->value_or_offset);
    }

    free(header_result);
    free(ifd_result);
    free(buffer);
    return 0;
}