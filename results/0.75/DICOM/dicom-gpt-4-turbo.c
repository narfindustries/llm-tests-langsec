#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the DICOM tags as per the DICOM standard
#define DICOM_PREAMBLE_SIZE 128
#define DICOM_PREFIX "DICM"
#define DICOM_PREFIX_SIZE 4

// Function prototypes
static HParser *dicom_parser();
static void parse_dicom_file(const char *filename);

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    parse_dicom_file(argv[1]);
    return 0;
}

// Parse a DICOM file
static void parse_dicom_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    // Move to the end of the file to find the file size
    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    rewind(file);

    // Allocate memory to contain the whole file
    uint8_t *buffer = (uint8_t *)malloc(filesize);
    if (!buffer) {
        perror("Memory error");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    // Copy the file into the buffer
    fread(buffer, 1, filesize, file);
    fclose(file);

    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, filesize);
    if (result) {
        printf("DICOM file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse DICOM file.\n");
    }

    // Cleanup
    free(buffer);
    h_parser_free(parser);
}

// Create a parser for DICOM files
static HParser *dicom_parser() {
    // Preamble: 128-byte of arbitrary data
    HParser *preamble = h_repeat_n(h_uint8(), DICOM_PREAMBLE_SIZE);

    // DICOM prefix: "DICM"
    HParser *prefix = h_token((const uint8_t *)DICOM_PREFIX, DICOM_PREFIX_SIZE);

    // Define how to parse a DICOM element
    HParser *tag_group = h_uint16();
    HParser *tag_element = h_uint16();
    HParser *vr = h_bits(16, false);
    HParser *length = h_uint32();
    HParser *value_field = h_repeat_n(h_uint8(), 1); // Placeholder, dynamically set in actual usage

    HParser *dicom_element = h_sequence(tag_group, tag_element, vr, length, h_indirect(), NULL);
    h_bind_indirect(dicom_element->seq.elements[4], h_repeat_n(h_uint8(), length));

    // DICOM elements are repeated until EOF
    HParser *dicom_elements = h_many(dicom_element);

    // Combine all parts into a single parser
    HParser *dicom_file = h_sequence(preamble, prefix, dicom_elements, NULL);

    return dicom_file;
}