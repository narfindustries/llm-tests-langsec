#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define JPEG markers
#define SOI_MARKER 0xFFD8
#define EOI_MARKER 0xFFD9
#define APP0_MARKER 0xFFE0
#define APP1_MARKER 0xFFE1
#define DQT_MARKER 0xFFDB
#define SOF0_MARKER 0xFFC0
#define SOF2_MARKER 0xFFC2
#define DHT_MARKER 0xFFC4
#define SOS_MARKER 0xFFDA
#define DRI_MARKER 0xFFDD
#define COM_MARKER 0xFFFE

// Helper to parse a marker
HParser *marker_parser(uint16_t marker) {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

// Define parsers for different segments
HParser *segment_parser() {
    return h_choice(
        marker_parser(SOI_MARKER),
        marker_parser(EOI_MARKER),
        marker_parser(APP0_MARKER),
        marker_parser(APP1_MARKER),
        marker_parser(DQT_MARKER),
        marker_parser(SOF0_MARKER),
        marker_parser(SOF2_MARKER),
        marker_parser(DHT_MARKER),
        marker_parser(SOS_MARKER),
        marker_parser(DRI_MARKER),
        marker_parser(COM_MARKER),
        NULL
    );
}

// Define JPEG parser
HParser *jpeg_parser() {
    return h_many(segment_parser());
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("JPEG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}