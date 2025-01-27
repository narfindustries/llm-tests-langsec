#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic types
static HParser *uint8 = h_uint8();
static HParser *int16 = h_int16();
static HParser *uint16 = h_uint16();
static HParser *int32 = h_int32();
static HParser *uint32 = h_uint32();
static HParser *string = h_length_value(uint16, h_uint8());

// DICOM Prefix: "DICM" at offset 128
static HParser *dicom_prefix = h_sequence(h_ignore(h_uint8(), 128), h_ch('D'), h_ch('I'), h_ch('C'), h_ch('M'), NULL);

// Tag: (group, element) pair
static HParser *tag = h_sequence(uint16, uint16, NULL);

// VR (Value Representation) - two characters
static HParser *vr = h_sequence(h_uint8(), h_uint8(), NULL);

// Length: Depending on VR, it can be 16-bit or 32-bit
static HParser *length = h_choice(h_bits(16, false), h_bits(32, false), NULL);

// Value Field: variable length, defined by Length
static HParser *value_field = h_length_value(length, h_uint8());

// Data Element
static HParser *data_element = h_sequence(tag, vr, length, value_field, NULL);

// DICOM File
static HParser *dicom_file = h_sequence(dicom_prefix, h_many(data_element), NULL);

int main(int argc, char **argv) {
    HParser *parser = dicom_file;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    return 0;
}