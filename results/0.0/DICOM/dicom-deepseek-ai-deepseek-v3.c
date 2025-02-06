#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define DICOM parser using Hammer combinators
HParser *dicom_parser() {
    // Define basic parsers for DICOM elements
    HParser *tag_parser = h_int16();  // Tag is 16-bit
    HParser *vr_parser = h_length_value(h_uint16(), h_ch_range('A', 'Z'));  // Value Representation is 2 uppercase letters
    HParser *length_parser = h_int16();  // Length is 16-bit
    HParser *value_parser = h_many(h_uint8());  // Value is a sequence of bytes

    // Combine parsers to form a DICOM element parser
    HParser *element_parser = h_sequence(tag_parser, vr_parser, length_parser, value_parser, NULL);

    // Define parser for the entire DICOM file
    HParser *dicom_file_parser = h_many(element_parser);

    return dicom_file_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
        return 1;
    }

    // Open the binary file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Read the entire file into memory
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *file_data = malloc(file_size);
    if (!file_data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }
    fread(file_data, 1, file_size, file);
    fclose(file);

    // Parse the DICOM file
    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, file_data, file_size);

    if (result) {
        printf("DICOM file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DICOM file.\n");
    }

    free(file_data);
    return 0;
}