#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define JPEG markers
#define SOI_MARKER 0xFFD8
#define EOI_MARKER 0xFFD9

// Define parsers for JPEG segments
HParser *marker_parser = h_uint16();

HParser *length_parser = h_uint16();

HParser *segment_parser = h_sequence(marker_parser, length_parser, h_data(h_length_value(length_parser, h_uint8())), NULL);

HParser *jpeg_parser = NULL;

void initialize_parsers() {
    jpeg_parser = h_sequence(h_uint16_val(SOI_MARKER), h_many(segment_parser), h_uint16_val(EOI_MARKER), NULL);
}

void parse_jpeg(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *file_data = malloc(file_size);
    if (!file_data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(file_data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(jpeg_parser, file_data, file_size);

    if (result) {
        printf("JPEG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG file.\n");
    }

    free(file_data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    initialize_parsers();
    parse_jpeg(argv[1]);

    return EXIT_SUCCESS;
}