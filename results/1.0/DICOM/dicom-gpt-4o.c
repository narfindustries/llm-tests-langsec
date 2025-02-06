#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t preamble[128];
    char prefix[4];
} DICOMFileHeader;

HParser *create_dicom_parser() {
    HParser *preamble = h_repeat_n(h_uint8(), 128);
    HParser *prefix = h_sequence(
        h_ch('D'), h_ch('I'), h_ch('C'), h_ch('M'),
        NULL
    );

    // Placeholder for tag, VR, length, and value parsing.
    HParser *tag = h_uint16();          // Group number and element number.
    HParser *vr = h_choice(
        h_token("PN", 2), h_token("DA", 2), h_token("TM", 2),
        h_token("CS", 2), h_token("LO", 2), h_token("UI", 2),
        NULL
    );
    HParser *length = h_uint32();       // Length of the data element value.
    HParser *value = h_length_value(length, h_uint8());

    HParser *element = h_sequence(tag, vr, length, value, NULL);

    // DICOM File Meta Information header and dataset
    HParser *dicom_file_header = h_sequence(preamble, prefix, NULL);
    
    HParser *dicom_parser = h_many(element);

    return h_sequence(dicom_file_header, dicom_parser, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = create_dicom_parser();
    HParseResult *result = h_parse(parser, data, file_size);
    if (result) {
        printf("DICOM file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse DICOM file\n");
    }

    free(data);
    h_parser_free(parser);

    return EXIT_SUCCESS;
}