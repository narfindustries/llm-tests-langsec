#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// JPEG Header Parse Specification
static HParser* jpeg_header;
static HParser* jpeg_marker;
static HParser* jpeg_segment;

// Define JPEG Markers
static const uint8_t JPEG_SOI = 0xD8;   // Start of Image
static const uint8_t JPEG_EOI = 0xD9;   // End of Image
static const uint8_t JPEG_APP0 = 0xE0;  // Application-specific marker
static const uint8_t JPEG_SOF0 = 0xC0;  // Start of Frame (Baseline DCT)

// JPEG Marker Parser
static HParser* parse_jpeg_marker() {
    return h_choice(
        h_literal_uint8(JPEG_SOI),
        h_literal_uint8(JPEG_EOI),
        h_literal_uint8(JPEG_APP0),
        h_literal_uint8(JPEG_SOF0),
        NULL
    );
}

// JPEG Segment Parser
static HParser* parse_jpeg_segment() {
    return h_sequence(
        h_literal_uint8(0xFF),  // Marker prefix
        parse_jpeg_marker(),
        h_end_p(),
        NULL
    );
}

// JPEG Header Parser
static HParser* parse_jpeg_header() {
    return h_many(parse_jpeg_segment());
}

// Main JPEG Parsing Function
int parse_jpeg(const uint8_t* data, size_t len) {
    jpeg_marker = parse_jpeg_marker();
    jpeg_segment = parse_jpeg_segment();
    jpeg_header = parse_jpeg_header();

    HParseResult* result = h_parse(jpeg_header, data, len);
    
    if (result && result->ast) {
        printf("Valid JPEG Header\n");
        h_parse_result_free(result);
        return 1;
    } else {
        printf("Invalid JPEG Header\n");
        return 0;
    }
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        fprintf(stderr, "Error reading file\n");
        free(buffer);
        return 1;
    }

    int result = parse_jpeg(buffer, read_size);
    free(buffer);

    return result ? 0 : 1;
}