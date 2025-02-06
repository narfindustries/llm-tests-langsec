#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} TIFFField;

static HParser *tiff_field_parser(void) {
    return h_sequence(
        h_uint16(), // tag
        h_uint16(), // type
        h_uint32(), // count
        h_uint32(), // value_offset
        NULL
    );
}

static HParser *tiff_parser(void) {
    // Note: The byte order should be handled more gracefully for a real-world scenario.
    HParser *header_parser = h_sequence(
        h_choice(
            h_token((const uint8_t *)"II", 2), // Little endian
            h_token((const uint8_t *)"MM", 2), // Big endian
            NULL
        ),
        h_uint16(), // TIFF magic number (42)
        h_uint32(), // Offset to first IFD
        NULL
    );

    HParser *ifd_parser = h_many(tiff_field_parser()); // Correct use of h_many for repeating fields

    return h_sequence(
        header_parser,
        ifd_parser,
        NULL
    );
}

void parse_tiff(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) { // Correct check for parse success
        printf("TIFF file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse TIFF file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser); // Correct function for freeing a parser
    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_tiff(argv[1]);
    return EXIT_SUCCESS;
}