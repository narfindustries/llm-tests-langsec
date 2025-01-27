#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the DICOM parser using Hammer
HParser *dicom_parser() {
    // Define the DICOM preamble (128 bytes of 0x00 followed by "DICM")
    HParser *preamble = h_sequence(
        h_repeat_n(h_uint8(), 128),
        h_sequence(h_ch('D'), h_ch('I'), h_ch('C'), h_ch('M'), NULL)
    );

    // Define the DICOM data element structure
    HParser *data_element = h_sequence(
        h_uint16(), // Group Number
        h_uint16(), // Element Number
        h_uint16(), // VR (Value Representation)
        h_uint16(), // Value Length
        h_repeat(h_uint8(), h_length_value(h_uint16())), // Value Field
        NULL
    );

    // Define the DICOM file structure
    HParser *dicom_file = h_sequence(
        preamble,
        h_repeat(data_element, h_end_p()),
        NULL
    );

    return dicom_file;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    // Read the DICOM file into memory
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

    // Parse the DICOM file
    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("DICOM file parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM file\n");
    }

    free(buffer);
    return 0;
}