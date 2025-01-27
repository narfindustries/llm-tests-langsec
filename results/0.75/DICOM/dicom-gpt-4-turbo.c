#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic DICOM Tags Definitions
static HParser *A = h_uint16();  // Attribute Tag
static HParser *VR = h_ch_range(0x41, 0x5A, 2); // Value Representation
static HParser *VL = h_uint32(); // Value Length
static HParser *VAL = h_bytes(VL); // Value Field

// DICOM File Meta Information
static HParser *meta_elem = h_sequence(A, VR, VL, VAL, NULL);
static HParser *meta_info = h_many(meta_elem);

// DICOM Data Set (simplified)
static HParser *data_elem = h_sequence(A, VL, VAL, NULL);
static HParser *data_set = h_many(data_elem);

// DICOM Preamble and Prefix
static HParser *preamble = h_ignore(h_bytes(128)); // 128-byte preamble
static HParser *prefix = h_token("DICM", 4);

// Complete DICOM File Structure
static HParser *dicom_file = h_sequence(
    preamble, 
    prefix, 
    meta_info, 
    data_set, 
    NULL
);

int main(int argc, char **argv) {
    HParser *parser = dicom_file;
    HParseResult *result = h_parse(parser, (uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    return 0;
}