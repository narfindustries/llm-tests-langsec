#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// DICOM Tag Structure Definition
typedef struct {
    uint16_t group;
    uint16_t element;
    char* value;
} DICOMTag;

// Parser for DICOM Tag
static HParser* dicom_tag_parser(void) {
    return h_sequence(
        h_uint16(),   // Group
        h_uint16(),   // Element
        h_optional(h_string_c()),  // Optional Value
        NULL
    );
}

// Validation function for DICOM Tag
static bool validate_dicom_tag(DICOMTag* tag) {
    return (tag->group != 0 && tag->element != 0);
}

// Main parsing function
static HParsedResult* parse_dicom(HParser* parser, const uint8_t* input, size_t length) {
    return h_parse(parser, input, length);
}

int main(int argc, char** argv) {
    h_init(NULL, NULL);

    // Create DICOM tag parser
    HParser* dicom_parser = dicom_tag_parser();

    // Example input data
    uint8_t sample_data[] = {0x00, 0x08, 0x00, 0x10, 'T', 'E', 'S', 'T'};

    // Parse DICOM data
    HParsedResult* result = parse_dicom(dicom_parser, sample_data, sizeof(sample_data));

    if (result && result->ast) {
        // Process parsed result
        DICOMTag* tag = (DICOMTag*)result->ast;
        printf("DICOM Tag: Group 0x%04X, Element 0x%04X\n", tag->group, tag->element);
    }

    // Cleanup
    h_parse_result_free(result);
    h_destroy_parser(dicom_parser);
    return 0;
}