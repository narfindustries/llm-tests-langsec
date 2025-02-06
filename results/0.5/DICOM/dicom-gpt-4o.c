#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for basic DICOM VR types
HParser *create_uint16_parser() {
    return h_uint16();
}

HParser *create_uint32_parser() {
    return h_uint32();
}

HParser *create_string_parser() {
    return h_many1(h_choice(h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch('-'), NULL));
}

// Define parsers for common DICOM fields
HParser *create_tag_parser() {
    return h_sequence(create_uint16_parser(), create_uint16_parser(), NULL);
}

HParser *create_length_parser() {
    return create_uint32_parser();
}

// Define a basic DICOM element parser
HParser *create_dicom_element_parser() {
    return h_sequence(create_tag_parser(), create_length_parser(), h_length_value(create_uint32_parser(), h_uint8()), NULL);
}

// Define a parser for DICOM file meta information
HParser *create_dicom_file_meta_parser() {
    return h_many(create_dicom_element_parser());
}

// Define a parser for the entire DICOM file
HParser *create_dicom_parser() {
    return h_sequence(
        h_ch(0x00), h_ch(0x01), h_ch(0x02), h_ch(0x03),  // Preamble
        h_ch(0x44), h_ch(0x49), h_ch(0x43), h_ch(0x4D),  // "DICM" prefix
        create_dicom_file_meta_parser(),                // File Meta Information
        h_many(create_dicom_element_parser()),          // Data Set
        NULL
    );
}

void parse_dicom_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *file_data = (unsigned char *)malloc(file_size);
    if (!file_data) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(file_data, 1, file_size, file);
    fclose(file);

    HParser *dicom_parser = create_dicom_parser();
    HParseResult *result = h_parse(dicom_parser, file_data, file_size);

    if (result) {
        printf("DICOM file parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM file.\n");
    }

    h_parser_free(dicom_parser);
    free(file_data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_dicom_file(argv[1]);
    return EXIT_SUCCESS;
}