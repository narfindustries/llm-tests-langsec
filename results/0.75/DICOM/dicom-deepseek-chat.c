#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the DICOM parser using Hammer
HParser *dicom_parser() {
    // Define the DICOM preamble (128 bytes of 0x00 followed by "DICM")
    HParser *preamble = h_sequence(
        h_repeat_n(h_uint8(), 128),
        h_token((uint8_t *)"DICM", 4),
        NULL
    );

    // Define a DICOM element (tag, VR, length, value)
    HParser *element = h_sequence(
        h_uint16(), // Group number
        h_uint16(), // Element number
        h_choice(   // Value Representation (VR)
            h_token((uint8_t *)"AE", 2),
            h_token((uint8_t *)"AS", 2),
            h_token((uint8_t *)"AT", 2),
            h_token((uint8_t *)"CS", 2),
            h_token((uint8_t *)"DA", 2),
            h_token((uint8_t *)"DS", 2),
            h_token((uint8_t *)"DT", 2),
            h_token((uint8_t *)"FL", 2),
            h_token((uint8_t *)"FD", 2),
            h_token((uint8_t *)"IS", 2),
            h_token((uint8_t *)"LO", 2),
            h_token((uint8_t *)"LT", 2),
            h_token((uint8_t *)"OB", 2),
            h_token((uint8_t *)"OD", 2),
            h_token((uint8_t *)"OF", 2),
            h_token((uint8_t *)"OL", 2),
            h_token((uint8_t *)"OW", 2),
            h_token((uint8_t *)"PN", 2),
            h_token((uint8_t *)"SH", 2),
            h_token((uint8_t *)"SL", 2),
            h_token((uint8_t *)"SQ", 2),
            h_token((uint8_t *)"SS", 2),
            h_token((uint8_t *)"ST", 2),
            h_token((uint8_t *)"TM", 2),
            h_token((uint8_t *)"UI", 2),
            h_token((uint8_t *)"UL", 2),
            h_token((uint8_t *)"UN", 2),
            h_token((uint8_t *)"UR", 2),
            h_token((uint8_t *)"US", 2),
            h_token((uint8_t *)"UT", 2),
            NULL
        ),
        h_uint32(), // Value length
        h_length_value(h_uint32(), h_uint8()), // Value field
        NULL
    );

    // Define the DICOM file as a sequence of preamble and elements
    return h_sequence(
        preamble,
        h_repeat(element),
        NULL
    );
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    // Parse the DICOM file
    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}