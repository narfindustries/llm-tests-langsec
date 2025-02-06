#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// JPEG Markers
#define SOI 0xFFD8
#define EOI 0xFFD9
#define SOF0 0xFFC0
#define DQT 0xFFDB
#define DHT 0xFFC4
#define SOS 0xFFDA
#define APP0 0xFFE0
#define COM 0xFFFE

// Utility functions
static void check_parser(HParser *p, const uint8_t *input, size_t length) {
    HParseResult *result = h_parse(p, input, length);
    if (result) {
        printf("Parsing successful!\n");
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }
}

static uint8_t* read_file(const char *filename, size_t *length) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = malloc(*length);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    if (fread(data, 1, *length, file) != *length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fclose(file);
    return data;
}

// JPEG parser
static HParser *jpeg_parser() {
    HParser *marker = h_uint16();
    HParser *length = h_uint16();
    HParser *data = h_repeat_n(h_uint8(), h_sub(length, h_size(2)));
    HParser *segment = h_sequence(marker, length, data, NULL);

    HParser *jpeg = h_many(segment);
    return jpeg;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length;
    uint8_t *data = read_file(argv[1], &length);

    HParser *p = jpeg_parser();
    check_parser(p, data, length);

    free(data);
    h_free_parser(p);
    return EXIT_SUCCESS;
}