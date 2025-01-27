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
        h_many(h_char()),  // Value 
        NULL
    );
}

// Validation Function
static bool validate_dicom_tag(DICOMTag* tag) {
    return (tag->group != 0 && tag->element != 0);
}

// Main Parsing Function
static HParsedResult* parse_dicom_data(HParser* parser, uint8_t* input, size_t length) {
    return h_parse(parser, input, length);
}

int main(int argc, char** argv) {
    // Initialize Hammer
    h_init();

    // Create DICOM Tag Parser
    HParser* dicom_parser = dicom_tag_parser();

    // Example DICOM data
    uint8_t sample_data[] = {0x00, 0x10, 0x00, 0x20, 'T', 'E', 'S', 'T'};
    
    // Parse DICOM data
    HParsedResult* result = parse_dicom_data(dicom_parser, sample_data, sizeof(sample_data));

    // Check parsing result
    if (result && result->ast) {
        // Extract parsed data
        DICOMTag parsed_tag = {
            .group = h_get_uint16(result->ast, 0),
            .element = h_get_uint16(result->ast, 1),
            .value = (char*)h_get_str(result->ast, 2)
        };

        // Validate tag
        if (validate_dicom_tag(&parsed_tag)) {
            printf("Valid DICOM Tag: Group=%04X, Element=%04X, Value=%s\n", 
                   parsed_tag.group, parsed_tag.element, parsed_tag.value);
        }

        // Free resources
        h_parse_result_free(result);
    }

    // Cleanup
    h_destroy();
    return 0;
}