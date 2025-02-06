#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} TiffField;

typedef struct {
    uint16_t byte_order;
    uint16_t version;
    uint32_t ifd_offset;
} TiffHeader;

HParser *tiff_header_parser() {
    return h_sequence(
        h_uint16(), // Byte order
        h_uint16(), // Version
        h_uint32()  // IFD offset
    );
}

HParser *tiff_field_parser() {
    return h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32()  // Value/Offset
    );
}

HParser *tiff_ifd_parser() {
    return h_many(tiff_field_parser());
}

HParser *tiff_parser() {
    return h_sequence(
        tiff_header_parser(),
        tiff_ifd_parser()
    );
}

void parse_tiff(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("TIFF parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TIFF\n");
    }

    free(buffer);
    h_arena_free(h_arena()); // Corrected to use h_arena_free
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_tiff(argv[1]);
    return EXIT_SUCCESS;
}