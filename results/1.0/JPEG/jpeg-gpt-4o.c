#include <hammer/hammer.h>
#include <stdio.h>

HParser *create_jpeg_parser() {
    // Define JPEG markers
    HParser *marker = h_choice(
        h_ch(0xFF, h_uint8()),
        NULL
    );

    // Define basic JPEG structure: marker + length + data
    HParser *segment = h_sequence(
        marker,
        h_uint16(),     // length of segment
        h_data(length - 2), // actual data accounting for length field
        NULL
    );

    // Define the JPEG file structure
    HParser *jpeg_parser = h_many1(segment);

    return jpeg_parser;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    HParser *jpeg_parser = create_jpeg_parser();
    HFile *file = h_file_open(argv[1], "rb");

    if (!file) {
        perror("Could not open file");
        return 1;
    }

    HParseResult *result = h_parse(jpeg_parser, file, NULL);

    if (result) {
        printf("JPEG file successfully parsed.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse JPEG file.\n");
    }

    h_file_close(file);
    h_parser_free(jpeg_parser);

    return 0;
}
