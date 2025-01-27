#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t tag_delim[] = {0x00, 0x00};

static HParser* init_dicom_parser(void) {
    // Basic building blocks
    HParser *hex_digit = h_in_range(hex_digit, '0', '9');
    HParser *alpha = h_in_range(alpha, 'A', 'Z');
    HParser *byte = h_uint8();
    
    // DICOM Tag format: (gggg,eeee)
    HParser *tag_group = h_repeat_n(hex_digit, 4);
    HParser *tag_element = h_repeat_n(hex_digit, 4);
    HParser *tag = h_sequence(
        h_ch('('),
        tag_group,
        h_ch(','),
        tag_element,
        h_ch(')'),
        NULL
    );

    // VR (Value Representation) - two uppercase chars
    HParser *vr = h_repeat_n(alpha, 2);
    
    // Value Length - 16-bit unsigned integer
    HParser *value_length = h_uint16();
    
    // Value Field - sequence of bytes up to length
    HParser *value_field = h_length_value(value_length, byte);
    
    // Data Element structure
    HParser *data_element = h_sequence(
        tag,
        vr,
        value_length,
        value_field,
        NULL
    );
    
    // Full DICOM sequence - one or more data elements
    HParser *dicom = h_many1(data_element);
    
    return dicom;
}

HParser* dicom_parser = NULL;

void init_parser(void) {
    dicom_parser = init_dicom_parser();
}

int main(int argc, char** argv) {
    init_parser();
    return 0;
}