#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function prototypes for creating parsers
HParser *create_uint16_parser();
HParser *create_uint32_parser();
HParser *create_string_parser(size_t length);
HParser *create_uid_parser(size_t length);
HParser *create_dicom_element_parser();
HParser *create_dicom_file_parser();

// Create parsers for DICOM fields
HParser *create_uint16_parser() {
    return h_uint16();
}

HParser *create_uint32_parser() {
    return h_uint32();
}

HParser *create_string_parser(size_t length) {
    return h_repeat_n(length, h_uint8());
}

HParser *create_uid_parser(size_t length) {
    return h_repeat_n(length, h_uint8());
}

// Create a parser for a DICOM data element
HParser *create_dicom_element_parser() {
    return h_sequence(
        create_uint16_parser(), // Group Number
        create_uint16_parser(), // Element Number
        create_string_parser(2), // Value Representation
        create_uint32_parser(), // Value Length
        h_length_value(create_uint32_parser(), h_uint8()), // Value Field
        NULL
    );
}

// Create a parser for a DICOM file
HParser *create_dicom_file_parser() {
    return h_many(create_dicom_element_parser());
}

// Function to parse a DICOM file
void parse_dicom_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *dicom_file_parser = create_dicom_file_parser();
    HParseResult *result = h_parse(dicom_file_parser, buffer, file_size);
    if (result) {
        printf("DICOM file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DICOM file.\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_dicom_file(argv[1]);

    return EXIT_SUCCESS;
}