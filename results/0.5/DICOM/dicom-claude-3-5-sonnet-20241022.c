#include <hammer/hammer.h>
#include <stdio.h>

const HParser* init_dicom_parser() {
    // DICOM elements
    HParser *tag = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *vr = h_token((const uint8_t*)"AE", 2);
    HParser *length = h_uint16();
    HParser *value = h_length_value(length, h_uint8());
    
    // DICOM element sequence
    HParser *element = h_sequence(tag, vr, length, value, NULL);
    
    // Full DICOM sequence with preamble and prefix
    HParser *preamble = h_repeat_n(h_uint8(), 128);
    HParser *prefix = h_token((const uint8_t*)"DICM", 4);
    HParser *elements = h_many1(element);
    
    return h_sequence(preamble, prefix, elements, NULL);
}

int main() {
    const HParser *parser = init_dicom_parser();
    
    // Parse input
    uint8_t input[] = {
        // 128-byte preamble
        0x00, 0x00, 0x00, 0x00, /* ... repeat to 128 bytes ... */
        // DICM prefix
        'D', 'I', 'C', 'M',
        // Sample element
        0x10, 0x00, 0x10, 0x00, // Tag (0010,0010)
        'A', 'E',               // VR
        0x0A, 0x00,            // Length
        'J', 'o', 'h', 'n', 'D', 'o', 'e', ' ', ' ', ' ' // Value
    };
    
    size_t input_size = sizeof(input);
    HParseResult *result = h_parse(parser, input, input_size);
    
    if(result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
    
    return 0;
}