#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NITF header structure
typedef struct {
    uint32_t file_header_length;
    char file_header[256];
    uint32_t image_segment_length;
    char image_segment[1024];
} NITFHeader;

// Define the Hammer parser for the NITF header
HParser *nitf_header_parser() {
    return h_sequence(
        h_int32(), // file_header_length
        h_length_value(h_int32(), h_whitespace(h_chars(256))), // file_header
        h_int32(), // image_segment_length
        h_length_value(h_int32(), h_whitespace(h_chars(1024))), // image_segment
        NULL
    );
}

// Function to parse the NITF file
NITFHeader *parse_nitf(const uint8_t *data, size_t length) {
    HParser *parser = nitf_header_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (!result) {
        fprintf(stderr, "Failed to parse NITF data\n");
        return NULL;
    }

    NITFHeader *header = (NITFHeader *)malloc(sizeof(NITFHeader));
    if (!header) {
        fprintf(stderr, "Memory allocation failed\n");
        h_parse_result_free(result);
        return NULL;
    }

    header->file_header_length = result->ast->seq->elements[0]->uint32;
    memcpy(header->file_header, result->ast->seq->elements[1]->str, 256);
    header->image_segment_length = result->ast->seq->elements[2]->uint32;
    memcpy(header->image_segment, result->ast->seq->elements[3]->str, 1024);

    h_parse_result_free(result);
    return header;
}

// Main function to test the parser
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    NITFHeader *header = parse_nitf(data, file_size);
    if (!header) {
        free(data);
        return 1;
    }

    printf("File Header Length: %u\n", header->file_header_length);
    printf("File Header: %s\n", header->file_header);
    printf("Image Segment Length: %u\n", header->image_segment_length);
    printf("Image Segment: %s\n", header->image_segment);

    free(header);
    free(data);
    return 0;
}