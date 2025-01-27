#include <hammer/hammer.h>

HParser *create_jpeg_parser() {
    // Define JPEG markers
    HParser *marker = h_choice(
        h_token("\xFF\xD8", 2), // SOI
        h_token("\xFF\xD9", 2), // EOI
        h_token("\xFF\xE0", 2), // APP0
        h_token("\xFF\xDB", 2), // DQT
        h_token("\xFF\xC0", 2), // SOF0
        h_token("\xFF\xC4", 2), // DHT
        h_token("\xFF\xDA", 2), // SOS
        NULL
    );

    // Define length field (2 bytes)
    HParser *length_field = h_uint16();

    // Define segment data
    HParser *segment_data = h_length_value(length_field, h_any_byte());

    // Define JPEG segment
    HParser *segment = h_sequence(marker, segment_data, NULL);

    // Define JPEG file structure
    HParser *jpeg_file = h_sequence(
        h_token("\xFF\xD8", 2), // SOI
        h_many1(segment),       // Segments
        h_token("\xFF\xD9", 2), // EOI
        NULL
    );

    return jpeg_file;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    // Create JPEG parser
    HParser *jpeg_parser = create_jpeg_parser();

    // Read JPEG file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse JPEG file
    HParseResult *result = h_parse(jpeg_parser, buffer, file_size);
    if (result) {
        printf("JPEG file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse JPEG file.\n");
    }

    // Cleanup
    free(buffer);
    h_parser_free(jpeg_parser);

    return 0;
}