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
static HParser* dicom_tag_parser() {
    return h_sequence(
        h_uint16(),   // Group
        h_uint16(),   // Element
        h_many(h_ch(' ')),
        h_optional(h_cstr()),  // Optional value
        NULL
    );
}

// Validation function for DICOM Tags
static bool validate_dicom_tag(DICOMTag* tag) {
    if (tag->group == 0x0002 && tag->element <= 0x0016) {
        return true;
    }
    return false;
}

// Main parsing function
static HParsedToken* parse_dicom_data(void* data) {
    HParser* parser = dicom_tag_parser();
    return h_parse(parser, data, strlen(data));
}

int main(int argc, char** argv) {
    // Initialize Hammer
    h_init(NULL, NULL);

    // Example DICOM tag data
    const char* sample_dicom_tag = "0002 0010 Transfer Syntax UID";

    // Parse DICOM tag
    HParsedToken* result = parse_dicom_data((void*)sample_dicom_tag);

    if (result) {
        // Extract parsed data
        DICOMTag tag = {
            .group = result->seq->elements[0]->uint,
            .element = result->seq->elements[1]->uint,
            .value = result->seq->elements[3] ? 
                     result->seq->elements[3]->str : NULL
        };

        // Validate tag
        if (validate_dicom_tag(&tag)) {
            printf("Valid DICOM Tag: Group 0x%04X, Element 0x%04X\n", 
                   tag.group, tag.element);
            if (tag.value) {
                printf("Value: %s\n", tag.value);
            }
        } else {
            printf("Invalid DICOM Tag\n");
        }

        // Free resources
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    return 0;
}