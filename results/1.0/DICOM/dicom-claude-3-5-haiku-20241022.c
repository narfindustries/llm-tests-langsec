#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// DICOM Tag Parsing Specification
static HParser* dicom_tag_parser(void) {
    HParser* tag_number = h_xint(16, 16, false);
    HParser* tag_name = h_many1(h_in_range('A', 'z'));
    HParser* tag_description = h_many1(h_in_range(' ', '~'));
    
    return h_sequence(tag_number, tag_name, tag_description, NULL);
}

// DICOM VR (Value Representation) Parser 
static HParser* dicom_vr_parser(void) {
    HStringArray* valid_vrs = h_str_array_new();
    h_str_array_append(valid_vrs, "AE");
    h_str_array_append(valid_vrs, "AS");
    h_str_array_append(valid_vrs, "AT");
    h_str_array_append(valid_vrs, "CS");
    h_str_array_append(valid_vrs, "DA");
    h_str_array_append(valid_vrs, "DS");
    h_str_array_append(valid_vrs, "DT");
    h_str_array_append(valid_vrs, "FL");
    h_str_array_append(valid_vrs, "FD");
    h_str_array_append(valid_vrs, "IS");
    h_str_array_append(valid_vrs, "LO");
    h_str_array_append(valid_vrs, "LT");
    h_str_array_append(valid_vrs, "OB");
    h_str_array_append(valid_vrs, "OD");
    h_str_array_append(valid_vrs, "OF");
    h_str_array_append(valid_vrs, "OL");
    h_str_array_append(valid_vrs, "OW");
    h_str_array_append(valid_vrs, "PN");
    h_str_array_append(valid_vrs, "SH");
    h_str_array_append(valid_vrs, "SL");
    h_str_array_append(valid_vrs, "SQ");
    h_str_array_append(valid_vrs, "SS");
    h_str_array_append(valid_vrs, "ST");
    h_str_array_append(valid_vrs, "TM");
    h_str_array_append(valid_vrs, "UC");
    h_str_array_append(valid_vrs, "UI");
    h_str_array_append(valid_vrs, "UL");
    h_str_array_append(valid_vrs, "UN");
    h_str_array_append(valid_vrs, "UR");
    h_str_array_append(valid_vrs, "US");
    h_str_array_append(valid_vrs, "UT");

    return h_in_array(valid_vrs);
}

// DICOM Element Parser
static HParser* dicom_element_parser(void) {
    HParser* tag = dicom_tag_parser();
    HParser* vr = dicom_vr_parser();
    HParser* length = h_int(32, 32, false);
    HParser* value = h_many(h_in_range(0, 255));
    
    return h_sequence(tag, vr, length, value, NULL);
}

// DICOM File Structure Parser 
static HParser* dicom_file_parser(void) {
    HParser* header = h_string("DICM");
    HParser* elements = h_many(dicom_element_parser());
    
    return h_sequence(header, elements, NULL);
}

int main(int argc, char** argv) {
    h_init(NULL, NULL);
    
    HParser* parser = dicom_file_parser();
    
    if (parser == NULL) {
        fprintf(stderr, "Failed to create DICOM parser\n");
        return 1;
    }
    
    // Parser can be used for further DICOM parsing tasks
    h_destroy(parser);
    
    return 0;
}