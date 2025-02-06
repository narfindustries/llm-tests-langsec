#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function for parsing DICOM element value based on its length
HParsedToken *dicom_value_parser_func(const HParseResult *p, void *env) {
    uint32_t length = *(uint32_t *)p->ast->bytes;
    return h_repeat_n(h_uint8(), length)->parse(p->input, p->input_length);
}

// Initialize parsers inside a function rather than as global variables
HParser *create_dicom_parsers() {
    // Define parsers for common data types in DICOM
    HParser *uint16_parser = h_uint16(); // Unsigned 16-bit integer
    HParser *uint32_parser = h_uint32(); // Unsigned 32-bit integer

    // Define a parser for DICOM tag (group, element)
    HParser *dicom_tag_parser = h_sequence(uint16_parser, uint16_parser, NULL);

    // Define a parser for DICOM Element Length
    HParser *dicom_length_parser = uint32_parser;

    // Define a parser for DICOM Element Value (variable length)
    HParser *dicom_value_parser = h_bind(dicom_length_parser, dicom_value_parser_func, NULL);

    // Define a parser for a complete DICOM Element
    HParser *dicom_element_parser = h_sequence(dicom_tag_parser, dicom_length_parser, dicom_value_parser, NULL);

    return dicom_element_parser;
}

// Main function to parse a DICOM file
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Error allocating buffer");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Create the parser
    HParser *dicom_element_parser = create_dicom_parsers();

    // Parse the buffer
    HParseResult *result = h_parse(dicom_element_parser, buffer, file_size);
    if (result) {
        printf("Parsed a DICOM element successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM element.\n");
    }

    free(buffer);
    return EXIT_SUCCESS;
}